
# Comments on the current design of fine-grained build hooks

## What additional arguments should be passed when executing rules?

When registering a preprocessor, you say "I care about `.x` files".
Then Cabal goes looking for `.x` files in various places, and passes
this information onto the preprocessor:
  - `(some search dir, filename)` for the `.x` file we found,
  - `(some build dir, filename)` for where to put the output.

In practice, Cabal goes searching in the following dirs:

```haskell
map getSymbolicPath (hsSourceDirs bi)
  ++ [ autogenComponentModulesDir lbi clbi
     , autogenPackageModulesDir lbi
     ]
```

and specifies an output in some build dir like

```haskell
let nm' = unUnqualComponentName nm
    flibDir = buildDir lbi </> nm' </> nm' ++ "-tmp"
```

or

```haskell
componentBuildDir lbi clbi = buildDir lbi </> nm'
```

This motivates the idea that, when declaring a rule, we should be able to
specify a dependency on e.g. a Haskell module, and when executing the rule
Cabal will resolve the dependency (by finding a base directory among its
search directories) and pass this information to the action that executes
the rule. The same logic applies to rule outputs, as the build-system chooses
where the outputs should live.

This is why we have:

```haskell
data Dependency = ProjectFile !FilePath
data ResultLocation = AutogenFile | BuildFile
type Result = ( ResultLocation, FilePath )
type ResolvedLocation = ( FilePath, FilePath )
```

Note that rules are only allowed to depend on file paths, and not directly on
other rules. The reason for this is two-fold:

  1. If a rule needs e.g. a certain `".chi"` file to exist, it is
     better to declare that upfront rather than indirectly by
     knowing which other rule generates it.
     This means the system is more robust, as if some internal detail changes
     which means that a different rule now generates the `".chi"` file,
     everything should continue to work.
  2. Because `RuleId`s are local names generated internally in the
     `RulesM` monad, if the rule that generates the file we need
     was defined somewhere else, then if we wanted to depend on the rule itself
     we would have to go search through existing rules to find the `RuleId` of
     the `Rule` that generates it; that would be rather indirect and annoying.

## Splitting off `Rule` and `Action`

Currently:

```haskell
data Rule =
  Rule
    { dependencies :: ![ Dependency ]
    , results :: ![ Result ]
    , actionId :: !ActionId
    }

data Action = Action { action :: [ ResolvedLocation ] -> ResultDirs -> IO () }
newtype ResultDirs = ResultDirs { resultDir :: ResultLocation -> FilePath }
```

If we remove the indirection `Rule -> ActionId -> Action`, and make explicit
the output of the action (by having it return the results rather than
writing them to disk in specific locations), this boils down to:

```haskell
data
  Rule
    { dependencies :: ![ Dependency ]
    , results :: ![ Result ]
    , runAction :: [ ResolvedLocation ] -- ^ locations of dependencies
                -> ResultDirs -- ^ directories in which the action results are expected
                -> IO [ ]
    }
```

with the length of dependencies matching.  
What's happening is that we declare dependencies, on either a path like
`"Lib.Mod.y"` or the output of another rule, and Cabal will resolve this
dependency, finding the full filepath of this dependency.

The `ResultDirs` tells the action where it should put its results. It would
be possible to follow the same structure as for the inputs, but in practice
this leads to a lot of redundant information being passed; so we opt instead
to simply pass some overall directories in which the different kinds of results
are expected.

### Action vs ActionId

The reason we are adding the indirection through `ActionId` is that it enables
running individual actions on demand.

Workflow when e.g. some files change on disk:

 - we recompute the rules (as the dependency information might have changed),
 - based on this new dependency information, we re-run the rules
   that have gone stale.

If a `Rule` directly stored the `Action`, we would have to serialise/deserialise
the actions (see the implementation of `hooksExecutable`), which would
complicate things further.

## Rules and names

The basic design of fine-grained rules is:

```haskell
data Rules inputs outputs
  = Rules
    { rules :: inputs -> IO (Map RuleId Rule)
    , actions :: inputs -> Map ActionId Action
    }
```

This allows us to separately compute rules and actions, for the two different
kinds of invocations of the separate hooks executable.

The `IO` in the return type of `rules :: inputs -> IO (Map RuleId Rule)` allows
us a limited form of dynamic dependencies. For example, we can query an external
tool such as `ghc -M` to determine dependency structure among source files, and
then use that information to generate the rules.
We still want the actions to be statically known, so that we can call out to
the separate hooks executable to run an action without repeatedly running the
`IO` action that is required to compute the rules.

One issue with this API is that it requires users to come up with names
themselves, which isn't robust when one wants to combine hooks. Instead, we
provide an API in which one registers actions and rules within a monad, so that
the name structure is correct by construction:

```haskell
newtype RulesM inputs outputs
  = RulesM { runRulesM :: inputs -> ActionsM ( RulesT IO outputs ) }

newtype FreshT x x_id m a = FreshT { freshT :: StateT ( Map x_id x ) m a }
type RulesT = FreshT Rule RuleId
type ActionsM = FreshT Action ActionId Identity

registerRule :: Monad m => Rule -> FreshT Rule RuleId m RuleId
registerAction :: Monad m => Action -> FreshT Action ActionId m ActionId
```

## Rule demand

HLS wants to know which rules need to rerun, and when.

The general flow is that we can find, by traversing the `Map RuleId Rule`
returned from querying all pre-build rules from the external hooks executable,
what all the dependencies of rules are (e.g. `Parser.y`).  
Whenever any of these changes, we must then:

  1. Re-query the pre-build rules to obtain all up-to-date rules. This step
     is necessary because the dependency structure might have changed.
  2. Find out all the rules that are now stale and need to be re-run.
  3. Re-run each of these rules by calling out to the separate hooks
     executable, passing the `ActionId` and additional action arguments to
     that executable in order to run the `Action` associated to each stale
     rule.

A rule is considered **out-of-date** if one of the following conditions applies:

  O1. an `unresolvedDependency` of the rule has been modified/created/deleted,
  O2. the environment passed to the rule has changed,
  O3. there has been a relevant change in a file or directory monitored
    by the rule, as specified by `monitoredFiles`.

If any rule is out-of-date, we re-run the computation that computes
all rules. Once this is done, we compute which rules are stale.

A rule is considered **stale** if, after re-running the computation of all
of the rules, one of the following conditions applies:

  S1. an `unresolvedDependency` of the rule has been
      modified/created/deleted,
  S2. the monitor value is stale, i.e. either:
    a. the `monitoredValue` of the rule changed, or
    b. the rule declares `monitoredValue = Nothing`,
  S3. a transitive dependency of the rule is stale.

A stale rule becomes no longer stale once we run its associated action; the
build system is responsible for re-running the actions associated with
each stale rule, in dependency order.

Justification:

  - (O1)/(S1) are clear. If we change a file that a rule depends on,
    the rule needs to be re-run. Because the dependency structure might also
    change, we need to recompute all the rules first, before then re-running
    the ones that have been staled.
  - (O2) is also clear: if we change the environment, we need to re-compute
    the rules as it is implemented as a function from an environment.
  - (S2) is an optimisation that ensures we don't pessimistically re-run a rule
    every time we change the environment, but only when the monitored value changes.
  - (O3) covers the use case in which we invoke an external tool (such as
    `ghc -M`) which performs a search in order to compute a dependency graph.
    We want to ensure that, if the user adds a new module, we
    re-run this dependency computation. We don't want to rely on the user
    necessarily re-configuring the package, especially as the package description
    might not necessarily have changed (even though in common cases one expects
    that adding a new source file would correspond to a new module declared in
    the cabal file).
