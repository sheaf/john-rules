-- |
-- Module: Rules
--
-- This module defines an API for fine-grained rules, e.g. pre-build rules
-- for generating source Haskell modules.
--
-- Rules are allowed to depend on source files (e.g. a source @.x@ file),
-- as well as on the output of other rules.
--
module Rules
  ( -- * Rules
    -- ** Rule
    Rule(..), simpleRule

    -- ** Rule inputs/outputs
  , UnresolvedDependency(..), Result, Location(..)

    -- ** Collections of rules
  , Rules(..), rules, noRules

    -- ** Monadic API for generation of 'ActionId'
  , FreshT(..), runFreshT, RulesM, ActionsM, RulesT
  , computeRules

    -- * Actions
  , Action(..), ActionFunction, simpleAction
  , ActionId(..)

    -- ** Resolved locations
  , ResolvedLocation, ResolvedLocations(..)
  , resolvedLocations, resolveResult

    -- * Pre-build rules
  , PreBuildRules

    -- * Placeholders (for the design phase)
  , SmallIO(..), BigIO(..)

  )
  where

-- base
import Control.Monad.Fix
  ( MonadFix )
import Data.Functor.Identity
  ( Identity(..) )
import qualified Data.List.NonEmpty as NE
import Data.Word
  ( Word64 )
import GHC.Generics
  ( Generic )

-- binary
import Data.Binary
  ( Binary )
import qualified Data.Binary as Binary
  ( get, put )

-- bytestring
import qualified Data.ByteString.Lazy as Lazy
  ( ByteString )

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( empty )

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS

-- john-rules
import CabalStubs
  ( PreBuildComponentInputs )
import Monitor
  ( MonitorFileOrDir )

--------------------------------------------------------------------------------

-- | A unique identifier for an t'Action'.
newtype ActionId = ActionId Word64
  deriving stock Show
  deriving newtype ( Eq, Ord, Binary )

-- | A monitored value.
--
-- Use this to declare a dependency on a certain projection of the
-- environment passed in to a computation.
--
-- A computation is considered out-of-date when the environment passed to
-- it changes and this value also changes.
--
-- A value of 'Nothing' means: always consider the computation to be out-of-date.
type MonitoredValue = Maybe Lazy.ByteString

-- | A rule consists of:
--
--  - an action to run, indirectly referenced by the 'ActionId' of
--    the 'Action'
--  - a description of the action:
--
--      - what inputs the action depends on,
--      - what outputs will result from the execution of the action.
--
-- Each 'UnresolvedDependency' will get resolved by the build system into a
-- 'ResolvedLocation', and these will be passed as additional arguments to the
-- action.
--
-- Use 'simpleRule' to construct a rule, overriding specific fields, rather
-- than directly using the 'Rule' constructor.
--
-- __Requirements:__ the t'Action' whose t'ActionId' is stored in the 'actionId'
-- field of a 'Rule' must satisfy the following:
--
--   - the t'Action' expects exactly as many 'ResolvedLocation' arguments
--     as there are 'UnresolvedDependency' values stored in the
--     'unresolvedDependencies' field of the 'Rule's;
--   - the execution of an action must produce precisely the 'Result's
--     specified by the 'results' field of the 'Rule'.
data Rule =
  -- | Please use the 'simpleRule' smart constructor instead of
  -- this constructor, in order to avoid relying on internal implementation
  -- details.
  Rule
  { unresolvedDependencies :: ![ UnresolvedDependency ]
     -- ^ Unresolved dependencies of this rule; see t'UnresolvedDependency'.
     --
     -- When the build system executes the action associated to this rule,
     -- it will resolve these dependencies and pass them as an argument
     -- to the action, in the form of a @['ResolvedLocation']@.
  , monitoredValue :: !MonitoredValue
     -- ^ A monitored value. The rule should be re-run whenever this value
     -- changes.
     --
     -- Use this to declare a dependency on a certain projection of the
     -- environment passed to the rule.
     --
     -- A value of 'Nothing' means: always re-run the rule when the
     -- environment passes to it changes.
  , monitoredFiles :: ![ MonitorFileOrDir FilePath Location ]
     -- ^ Monitored files or directories; if these change in some way,

  , results :: !( NE.NonEmpty Result )
     -- ^ Results of this rule; see t'Result'.
  , actionId :: !ActionId
     -- ^ To run this rule, which t'Action' should we execute?
     --
     -- The t'Action' will receive exactly as many 'ResolvedLocation'
     -- arguments as there are 'UnresolvedDependency' values stored in
     -- the 'unresolvedDependencies' field.
  }
  deriving stock ( Generic, Show )
  deriving anyclass Binary

-- | A simple rule, with no additional monitoring.
--
-- Prefer using this smart constructor instead of v'Rule' whenever possible.
simpleRule :: ActionId -> [UnresolvedDependency] -> NE.NonEmpty Result -> Rule
simpleRule actId dep res =
  Rule { unresolvedDependencies = dep
       , results = res
       , monitoredValue = Nothing
       , monitoredFiles = []
       , actionId = actId
       }

-- | A dependency of a rule, which needs to be resolved by the build system
-- in some way.
data UnresolvedDependency
  -- | Declare a dependency on a file from the current project that should
  -- be found by looking at project search paths.
  --
  -- This file might exist already, or it might be the output of another rule.
  = ProjectSearchDirFile
      !Location
        -- ^ where to go looking for the file
      !FilePath
        -- ^ path of the file, relative to a Cabal search directory
  deriving stock ( Generic, Show )
  deriving anyclass Binary

-- | A description of the location of a file generated by executing a rule.
type Result = ( Location, FilePath )

-- | A description of the directory in which we expect a particular file to
-- be located, whether the file be an input or an output of a rule.
data Location
  -- | A source file:
  --
  -- - for a rule dependency, we will go looking for it in
  --   the source directories and in autogen modules directories;
  -- - for a rule output, the file should be put in an autogen module directory.
  = SrcFile
  -- | A build-file, that belongs to some build directory.
  | BuildFile
  -- | A temporary file.
  | TmpFile
  deriving stock ( Generic, Show, Eq, Ord, Enum, Bounded )
  deriving anyclass Binary

-- | A resolved location of a dependency or result of a rule, consisting
-- of an absolute path and of a file path relative to that base path.
--
-- In practice, this will be something like @( dir, toFilePath modName )@,
-- where:
--
--  - for a dependency, @dir@ is one of the Cabal search directories,
--  - for an output, @dir@ is a directory such as @autogenComponentModulesDir@
--    or @componentBuildDir@.
type ResolvedLocation = ( FilePath, FilePath )
  -- The reason for splitting it up this way is that some pre-processors don't
  -- simply generate one output @.hs@ file from one input file, but have
  -- dependencies on other generated files (notably @c2hs@, where building one
  -- @.hs@ file may require reading other @.chi@ files, and then compiling the
  -- @.hs@ file may require reading a generated @.h@ file).
  -- In these cases, the generated files need to embed relative path names to each
  -- other (eg the generated @.hs@ file mentions the @.h@ file in the FFI imports).
  -- This path must be relative to the base directory where the generated files
  -- are located; it cannot be relative to the top level of the build tree because
  -- the compilers do not look for @.h@ files relative to there, ie we do not use
  -- @-I .@, instead we use @-I dist/build@ (or whatever dist dir has been set
  -- by the user).

-- | A function that specifies how to resolve a t'Location' to a specific
-- absolute directory path.
--
-- See 'resolveResult'.
newtype ResolvedLocations =
  ResolvedLocations { resolveLocation :: Location -> FilePath }
locations :: [Location]
locations = [ minBound .. maxBound ]
resolvedLocations :: ResolvedLocations -> [FilePath]
resolvedLocations (ResolvedLocations { resolveLocation = fn })
  = map fn locations

-- | Utility to resolve a t'Result' into a t'ResolvedLocation' using
-- 'ResolvedLocations'.
--
-- Typically used to determine the t'ResolvedLocation's of the t'Result's of a
-- rule in the context of an 'Action'.
resolveResult :: ResolvedLocations -> Result -> ResolvedLocation
resolveResult ResolvedLocations{resolveLocation} (loc, fp) = (resolveLocation loc, fp)

instance Show ResolvedLocations where
  show dirs = show $ resolvedLocations dirs
instance Eq ResolvedLocations where
  f == g = resolvedLocations f == resolvedLocations g
instance Ord ResolvedLocations where
  compare f g = compare (resolvedLocations f) (resolvedLocations g)
instance Binary ResolvedLocations where
  put f = mapM_ Binary.put $ resolvedLocations f
  get = do
    -- NB: be careful to ensure that the order here
    -- matches the Enum instance for 'ResultLocation'
    srcDir   <- Binary.get
    buildDir <- Binary.get
    tmpDir   <- Binary.get
    return $ ResolvedLocations $ \ loc -> case loc of
      SrcFile   -> srcDir
      BuildFile -> buildDir
      TmpFile   -> tmpDir

type ActionFunction =
  [ ResolvedLocation ]
           -- ^ locations of the dependencies of this action,
           -- as declared by the rule that this action is executing
      -> ResolvedLocations
           -- ^ directories in which the results of this action
           -- should be put

           -- API design note: this argument could instead have type
           -- [ ResolvedDependency ], and we would pass as many paths as
           -- the rule that this action is executing declares results,
           -- but that would mean that if the rule has many outputs,
           -- the action would need to be able to handle putting each of its
           -- outputs in a different directory. This is rather unergonomic,
           -- and it would require passing all these filepaths through the
           -- SetupHooks CLI interface.
      -> BigIO ()

-- | An action to run to execute a 'Rule', for example an invocation
-- of an external executable such as @happy@ or @alex@.
--
-- Use 'simpleAction' to construct an action, overriding specific fields,
-- rather than directly using the 'Action' constructor.
--
-- Arguments:
--
--  1. The locations of the __dependencies__ of this action,
--     as declared by the rule that this action is executing.
--     There will be as many values passed in this list as there are
--     'UnresolvedDependency' values declared in the 'Rule' that calls
--     the action.
--  2. Directories in which the __results__ of this action should be put.
--     See 'ResolveLocations' and 'resolveResult'.
newtype Action =
  -- | Please use the 'simpleAction' smart constructor instead of
  -- this constructor, in order to avoid relying on internal implementation
  -- details.
  Action { action :: ActionFunction }

-- | A simple action, with no additional monitoring.
--
-- Prefer using this smart constructor over v'Action' whenever possible.
simpleAction :: ActionFunction -> Action
simpleAction f = Action { action = f }

-- | A collection of t'Rule's and t'Action's for executing them.
--
-- Use the 'rules' smart constructor instead of directly using the v'Rules'
-- constructor.
--
-- Actions are registered using 'registerAction', and rules are registered
-- using 'registerRule'.
--
-- The @env@ type parameter represents an extra argument, which usually
-- consists of information known to Cabal such as 'LocalBuildInfo' and
-- 'ComponentLocalBuildInfo'.
--
-- Even though t'Rule's and t'Action's are registered separately, the nature
-- of the arguments passed to an t'Action' corresponds with the description
-- of the 'Rule', e.g. the action will receive as many 'ResolvedLocations'
-- in its first argument as the rule declared 'UnresolvedDependency' values.
-- See t'Rule' for more information.
newtype Rules env =
  Rules { runRules :: env -> RulesM () }
    -- ^ Return a collection of t'Action's and their t'ActionId's, and then,
    -- in the inner computation, a collection of t'RuleId's, with each t'RuleId'
    -- associated with a corresponding 'Rule' which specifies its dependencies
    -- as well as the t'ActionId' of the t'Action' to run in order to execute
    -- the rule.
    --
    -- You can think of this type signature as:
    --
    -- > env -> ( Map ActionId Action, IO ( Map RuleId Rule ) )
    --
    -- except that it is structured in such a way as to avoid having
    -- to manually create t'ActionId' and t'RuleId' values.

-- | __Warning__: this 'Semigroup' instance is not commutative.
instance Semigroup ( Rules env ) where
  ( Rules rs1 ) <> ( Rules rs2 ) =
    Rules $ \ inputs -> do
      x1 <- rs1 inputs
      x2 <- rs2 inputs
      return $ do
         y1 <- x1
         y2 <- x2
         return $ y1 <> y2

instance Monoid ( Rules env ) where
  mempty = Rules $ const noRules

-- | An empty collection of rules.
noRules :: RulesM ()
noRules = pure $ pure ()

-- | Construct a collection of rules.
--
-- Prefer using this smart constructor over v'Rules' whenever possible.
rules :: ( env -> RulesM () ) -> Rules env
rules f = Rules { runRules = f }

-- | Internal function: run the monadic 'Rules' computations in order
-- to obtain all the 'Action's and 'Rule's.
computeRules
  :: env
  -> Rules env
  -> ( Map ActionId Action, SmallIO [Rule] )
computeRules inputs ( Rules rs ) =
  ( actionFromId, execWriterT getRules )
  where
    ( getRules, actionFromId ) = runIdentity $ runFreshT $ rs inputs

--------------------------------------------------------------------------------
-- API for keeping track of all declared rules

-- | Monadic API for constructing rules.
type RulesM a = ActionsM ( RulesT SmallIO a )

-- | Monad transformer for defining rules. Usually wraps the 'IO' monad,
-- allowing @IO@ actions to be performed using @liftIO@.
type RulesT = WriterT [Rule]

-- These newtypes are placeholders meant to help keep track of effects
-- during the design of the API.
newtype BigIO a = BigIO { runBigIO :: IO a }
  deriving newtype ( Functor, Applicative, Monad )
newtype SmallIO a = SmallIO { runSmallIO :: IO a }
  deriving newtype ( Functor, Applicative, Monad )

--------------------------------------------------------------------------------
-- API for ActionIds.

-- | A monad transformer for the registration of values, through the creation
-- of fresh identifiers for these values.
--
-- In practice, you should use 'ActionsM', which is specialised to the creation
-- of 'ActionId' identifiers.
newtype FreshT x x_id m a = FreshT { freshT :: StateT ( Map x_id x ) m a }
  deriving newtype ( Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix )
runFreshT :: FreshT x x_id m a -> m ( a, Map x_id x )
runFreshT = ( `runStateT` Map.empty ) . freshT

-- | Monad for defining actions. Does not support any 'IO'.
type ActionsM = FreshT Action ActionId Identity

--------------------------------------------------------------------------------
-- Rules for the pre-build phase (preparing sources).

-- | PreBuildRules declare how a certain collection of modules in a
-- given component of a package will be generated.
type PreBuildRules = Rules PreBuildComponentInputs
