-- |
-- Module: ExternalHooks
--
-- This module tests that the Rules API is fit for purpose, i.e. that we can
-- properly implement a separate hooks executable and a build system on the
-- back of the design.
module ExternalHooks
  ( hooksExecutable
  , executeRules
  )
  where

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
import qualified Data.Map.Strict as Map
  ( lookup )

-- john-rules
import Monitor
  ( MonitorFileOrDir(..) )
import Rules

--------------------------------------------------------------------------------
-- External hooks executable, and associated build system.

-- | Create an executable which accepts the name of a hook as the argument,
-- then reads arguments to the hook over stdin and writes the results of the hook
-- to stdout.
hooksExecutable :: PreBuildRules -> IO ()
hooksExecutable pbRules = do
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
              let (_, ruleFromId) = computeRules cabalBuildInfoStuff pbRules
              runSmallIO ruleFromId
        "runPreBuildAction" ->
          -- Execute a pre-build action, given its ActionId and ActionArg.
          runHookHandle
            \ (cabalBuildInfoStuff, actId, depLocs, resLocs) -> do
            let (actionFromId, _) = computeRules cabalBuildInfoStuff pbRules
            case Map.lookup actId actionFromId of
              Nothing -> error $ "hooksExecutable: no such action " ++ show actId
              Just ( Action f ) ->
                runBigIO $ f depLocs resLocs
        _ -> error $ "hooksExecutable: invalid hook name " ++ hookName

executeRules
  :: Rules inputs
  -> inputs
  -> IO ()
executeRules rulesFromInputs inputs = do
  let (actionFromId, getRules) = computeRules inputs rulesFromInputs
  -- Get all the rules, and create a build graph out of them.
  allRules <- zip [0..] <$> runSmallIO getRules
  let
    ( ruleGraph, ruleFromVertex, _vertexFromRuleId ) =
      Graph.graphFromEdges
        [ ( rule, rId, getRuleDependencies rule )
        | ( rId, rule ) <- allRules
        ]

    -- All the other rules that a given rule directly depends on, as determined
    -- by its 'unresolvedDependencies' and 'monitored' fields.
    getRuleDependencies :: Rule -> [ Int ]
    getRuleDependencies
      ( Rule { unresolvedDependencies = deps
             , monitoredFiles = mons } )
      = nub $ concat $
          [ mapMaybe
            ( \ ( rId, r ) -> if r `ruleOutputsPath` fp then Just rId else Nothing )
              allRules
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
              allRules
          | mon <- mons ]

    resolveDep :: UnresolvedDependency -> IO ResolvedLocation
    resolveDep = \case
      ProjectSearchDirFile _ fp -> do
        basePath <- return "src" -- search in search paths
        return (basePath, fp)
    resDirs :: ResolvedLocations
    resDirs = ResolvedLocations \case
      SrcFile ->
        "autogenComponentModulesDir"
      BuildFile ->
        "componentBuildDir"
      TmpFile ->
        "componentTmpDir"

  for_ ( Graph.reverseTopSort ruleGraph ) \ v -> do
    let ( Rule { actionId = actId, unresolvedDependencies = deps }, ruleId, _ )
          = ruleFromVertex v
    case Map.lookup actId actionFromId of
      Nothing ->
        -- This is an internal error, because we don't expose the constructor
        -- of 'ActionId' through the SetupHooks interface, so it shouldn't
        -- be possible to conjure up invalid 'ActionId's (unless one is
        -- deliberately trying to cause trouble, e.g. by using unsafeCoerce).
        error $ unlines
          [ "Internal error when trying to execute rule " ++ show ruleId ++ ":"
          , "there is no Action with " ++ show actId ++ "." ]
      Just (Action { action = execRule }) -> do
        -- Run the rule, passing it the resolved locations it expects.
        resolvedDeps <- traverse resolveDep deps
        runBigIO $ execRule resolvedDeps resDirs

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
