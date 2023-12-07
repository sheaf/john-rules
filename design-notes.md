
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
data Dependency
  = ProjectFile !FilePath
data Result
  = AutogenFile !FilePath
  | BuildFile !FilePath
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

data Action = Action { action :: [ ResolvedLocation ] -> [ ResolvedLocation ] -> IO () }
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
                -> [ ResolvedLocation ] -- ^ desired locations of results
                -> IO [ ]
    }
```

with the length of dependencies and of results matching.  
What's happening is that we declare dependencies, on either a path like
`"Lib.Mod.y"` or the output of another rule, and Cabal will resolve this
dependency, finding the full filepath of this dependency.

For results, we promise ahead of time to only write files in two places:

  - in the correct `autogenComponentModulesDir`, e.g. when we're generating a `.hs` file,
  - in the correct `componentBuildDir`, e.g. when we're generating `.chi` files
    as part of running `c2hs` on a bunch of `.chs` files that depend on eachother.

The reason we are adding this indirection is that it enables running individual
actions on demand.

Workflow when e.g. some files change on disk:

 - we recompute the rules (as the dependency information might have changed),
 - based on this new dependency information, we re-run the rules
   that have gone stale.

We want to be able to execute the rules of our choice; so in the `Rules`
datatype, we separately compute `Action`s and `Rule`s:

```haskell
data Rules inputs outputs
  = Rules
  { rules :: inputs -> RulesM outputs
  , actions :: inputs -> Map ActionId Action
  }
```

If we didn't do this, and instead a `Rule` directly stored the `Action`,
we would have to serialise/deserialise the actions (see the implementation
of `hooksExecutable`), which would complicate things further.
