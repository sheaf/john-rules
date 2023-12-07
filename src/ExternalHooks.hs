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
import Data.Maybe
  ( listToMaybe, mapMaybe )
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
  ( assocs, lookup )

-- john-rules
import CabalStubs
  ( ModuleName, PreBuildComponentInputs )
import Rules

--------------------------------------------------------------------------------
-- External hooks executable, and associated build system.

-- | Create an executable which accepts the name of a hook as the argument,
-- then reads arguments to the hook over stdin and writes the results of the hook
-- to stdout.
hooksExecutable :: PreBuildRules -> IO ()
hooksExecutable ( Rules { rules, actions} ) = do
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
              runSmallIO $ runRulesM $ rules cabalBuildInfoStuff
        "runPreBuildAction" ->
          -- Execute a pre-build action, given its ActionId and ActionArg.
          runHookHandle
            \ (cabalBuildInfoStuff, actId, actionArgData) -> do
            let allActions = actions cabalBuildInfoStuff
            case Map.lookup actId allActions of
              Nothing -> error $ "hooksExecutable: no such action " ++ show actId
              Just ( Action f ) ->
                runBigIO $ f ( Binary.decode actionArgData )
        _ -> error $ "hooksExecutable: invalid hook name " ++ hookName

runPreBuildRules
  :: PreBuildComponentInputs
  -> ( PreBuildComponentInputs -> ActionId -> [ResolvedDependency] -> IO () )
    -- ^ how to run an individual action
  -> ( PreBuildComponentInputs -> IO ( Map ModuleName RuleId, Map RuleId Rule ) )
    -- ^ all pre-build rules
  -> IO ()
runPreBuildRules buildInfoStuff runAction getAllRules = do
  ( _modRules, ruleFromId ) <- getAllRules buildInfoStuff
  let
    ( ruleGraph, ruleFromVertex, _vertexIdFromRuleId ) =
      Graph.graphFromEdges
        [ ( rule, rId, ruleDeps )
        | ( rId, rule@( Rule { dependencies = allDeps } ) ) <- Map.assocs ruleFromId
        , let ruleDeps =
                mapMaybe
                  ( \case { RuleResult ( RuleResultRef { ruleId = i } ) -> Just i; ProjectFile {} -> Nothing} )
                   allDeps
        ]
    resolveDep :: RuleId -> Dependency -> IO ResolvedDependency
    resolveDep baseRuleId = \case
      ProjectFile fp -> do
        basePath <- case buildInfoStuff of { _ -> return "src" } -- TODO: look for fp in the search paths and find the base path
        return (basePath, fp)
      RuleResult ( RuleResultRef { ruleId = depRuleId, ruleResultIndex = i } ) ->
        case Map.lookup depRuleId ruleFromId of
          Nothing -> error $ "runPreBuildRules: rule " ++ show baseRuleId ++ " depends on non-existent rule " ++ show depRuleId
          Just ( Rule { results = depResults }) ->
            case listToMaybe $ drop ( fromIntegral i ) depResults of
              Nothing -> error $ unlines
                [ "runPreBuildRules: rule " ++ show baseRuleId ++ " depends on output " ++ show i ++ "of rule " ++ show depRuleId ++ ","
                , "but that rule only has " ++ show (length depResults) ++ " outputs." ]
              Just res -> case res of
                AutogenFile fp -> return ("autogenComponentModulesDir" {- stub -}, fp)
                BuildFile fp -> return ("componentBuildDir" {- stub -}, fp)

  for_ ( Graph.reverseTopSort ruleGraph ) \ v -> do
    let ( Rule { actionId = actId, dependencies = deps }, rId, _ ) = ruleFromVertex v
    resolvedDeps <- traverse (resolveDep rId) deps
    runAction buildInfoStuff actId resolvedDeps

  -- On-demand recompilation: when some of the 'ProjectFile' inputs
  -- specified in the 'ruleFromId' map are modified:
  --   - rerun 'getRules' (as the dependency graph may have changed)
  --   - execute the part of the build graph that is now stale

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
