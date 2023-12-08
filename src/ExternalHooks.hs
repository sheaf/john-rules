-- |
-- Module: ExternalHooks
--
-- This module tests that the Rules API is fit for purpose, i.e. that we can
-- properly implement a separate hooks executable and a build system on the
-- back of the design.
module ExternalHooks where

-- base
import Data.Foldable
  ( for_ )
import Data.List
  ( nub )
import Data.Maybe
  ( mapMaybe )
import System.Environment
  ( getArgs )

-- binary
import Data.Binary
  ( Binary )
import qualified Data.Binary as Binary
  ( encode, decode )

-- bytestring
import qualified Data.ByteString.Lazy as LBS
  ( getContents, putStr )

-- containers
import qualified Data.Graph as Graph
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( assocs, lookup, toList )

-- john-rules
import CabalStubs
  ( PreBuildComponentInputs )
import Monitor
  ( MonitorFileOrDir(..) )
import Rules

--------------------------------------------------------------------------------
-- External hooks executable, and associated build system.

-- | Create an executable which accepts the name of a hook as the argument,
-- then reads arguments to the hook over stdin and writes the results of the hook
-- to stdout.
hooksExecutable :: PreBuildRules -> IO ()
hooksExecutable ( Rules { rules, actions } ) = do
  args <- getArgs
  case args of
    [] -> error "hooksExecutable: missing argument"
    hookName:_ ->
      case hookName of
        "preBuildRules" ->
          -- Query all pre-build rules.
          -- Returns:
          --   - a map that says which rule to run to generate it (Map ModuleName RuleId)
          --   - for each rule, what its dependencies are, and what Action to run to execute it
          runHookHandle
            \ cabalBuildInfoStuff -> do
              ruleFromId <- runSmallIO $ rules cabalBuildInfoStuff
              return ruleFromId
        "runPreBuildAction" ->
          -- Execute a pre-build action, given its ActionId and ActionArg.
          runHookHandle
            \ (cabalBuildInfoStuff, actId, depLocs, resLocs) -> do
            let allActions = actions cabalBuildInfoStuff
            case Map.lookup actId allActions of
              Nothing -> error $ "hooksExecutable: no such action " ++ show actId
              Just ( Action f ) ->
                runBigIO $ f depLocs resLocs
        _ -> error $ "hooksExecutable: invalid hook name " ++ hookName

runPreBuildRules
  :: PreBuildComponentInputs
  -> ( PreBuildComponentInputs -> ActionId -> ActionFunction )
    -- ^ how to run an individual action
  -> ( PreBuildComponentInputs -> IO ( Map RuleId Rule ) )
    -- ^ all pre-build rules
  -> IO ()
runPreBuildRules buildInfoStuff runAction getAllRules = do
  ruleFromId <- getAllRules buildInfoStuff
  let
    ( ruleGraph, ruleFromVertex, _vertexIdFromRuleId ) =
      Graph.graphFromEdges
        [ ( rule, rId, getRuleDependencies rule )
        | ( rId, rule ) <- Map.assocs ruleFromId
        ]

    -- All the other rules that a given rule directly depends on, as determined
    -- by its 'unresolvedDependencies' and 'monitored' fields.
    getRuleDependencies :: Rule -> [ RuleId ]
    getRuleDependencies
      ( Rule { unresolvedDependencies = deps
             , monitoredFiles = mons } )
      = nub $ concat $
          [ mapMaybe
            ( \ ( rId, r ) -> if r `ruleOutputsPath` fp then Just rId else Nothing )
              ( Map.toList ruleFromId )
          | dep <- deps
          , let fp = case dep of { ProjectSearchDirFile _ path -> path } ]
          ++
          [ mapMaybe
            ( \ ( rId, r ) ->
                case mon of
                  MonitorFile _ fp
                    | r `ruleOutputsPath` fp
                    -> Just rId
                  MonitorDirContents loc
                    | r `ruleOutputsAtLocation` loc
                    -> Just rId
                  _ -> Nothing )
              ( Map.toList ruleFromId )
          | mon <- mons ]

    resolveDep :: UnresolvedDependency -> IO ResolvedLocation
    resolveDep = \case
      ProjectSearchDirFile _ fp -> do
        basePath <- case buildInfoStuff of { _ -> return "src" } -- search in search paths
        return (basePath, fp)
    resDirs :: ResolvedLocations
    resDirs = ResolvedLocations \case
      SrcFile ->
        "autogenComponentModulesDir"
      BuildFile ->
        "componentBuildDir"

  for_ ( Graph.reverseTopSort ruleGraph ) \ v -> do
    let ( Rule { actionId = actId, unresolvedDependencies = deps }, _, _ )
          = ruleFromVertex v
    resolvedDeps <- traverse resolveDep deps
    runBigIO $ runAction buildInfoStuff actId resolvedDeps resDirs

  -- On-demand recompilation: when some of the 'ProjectFile' inputs
  -- specified in the 'ruleFromId' map are modified:
  --   - rerun 'getRules' (as the dependency graph may have changed)
  --   - execute the part of the build graph that is now stale
  --
  -- Note that this does not handle e.g. adding a new .chs file entirely;
  -- in that case we expect the user to re-configure first.

-- | Does the rule output the given file?
ruleOutputsPath :: Rule -> FilePath -> Bool
ruleOutputsPath ( Rule { results = rs } ) fp = any ( (== fp) . snd ) rs

-- | Does the rule output files at the given location?
ruleOutputsAtLocation :: Rule -> Location -> Bool
ruleOutputsAtLocation ( Rule { results = rs } ) loc = any ( (== loc) . fst ) rs

-- | Run a hook executable, passing inputs via stdin
-- and getting results from stdout.
runHookHandle
  :: ( Binary inputs, Binary outputs )
  => ( inputs -> IO outputs )
  -- ^ Hook to run; inputs are passed via stdin
  -> IO ()
runHookHandle hook = do
  inputsData <- LBS.getContents
  let inputs = Binary.decode inputsData
  output <- hook inputs
  LBS.putStr $ Binary.encode output
