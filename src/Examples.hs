{-# LANGUAGE OverloadedStrings #-}

module Examples where

-- base
import Control.Monad.Fix
  ( mfix )
import Data.Functor.Identity
  ( Identity(..) )

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( (!), fromList, traverseWithKey )
import Data.Set
  ( Set )
import qualified Data.Set as Set
  ( empty, fromList, toList)

-- filepath
import System.FilePath
  ( (<.>) )

-- john-rules
import CabalStubs
  ( ModuleName(..), PreBuildComponentInputs
  , toFilePath
  )
import Rules

--------------------------------------------------------------------------------
-- Example usage of the API.

-- Example 1: generating modules ex-nihilo (e.g. stack)

stackRules :: PreBuildRules
stackRules = Rules
  { actions = \ buildInfoStuff ->
      let
        genBuildModAction =
          Action $ \ _ -> writeStackBuildFile buildInfoStuff
      in Map.fromList [ ( genBuildModActionId, genBuildModAction ) ]
  , rules = \ buildInfoStuff -> do
      genStackBuildRuleId <- registerRule $
        Rule
          { dependencies = []
          , actionId = genBuildModActionId
          , results = [ AutogenFile $ toFilePath (stackBuildModule buildInfoStuff) ]
          }
      return $ Map.fromList
        [ (stackBuildModule buildInfoStuff, genStackBuildRuleId) ]
  }
  where
    genBuildModActionId = ActionId 1

-- Example 2: preprocessing using c2hs.

chsRules :: PreBuildRules
chsRules = Rules
  { actions = \ buildInfoStuff ->
      let
        runChsAction =
          Action $ \ (chsFileLoc:_chiFileLocs) ->
            runChs buildInfoStuff chsFileLoc
      in Map.fromList [ ( chsActionId, runChsAction ) ]
  , rules = \ buildInfoStuff -> do
      chsGraph <- liftFresh $ callChsForDeps buildInfoStuff
      hoist ( pure . runIdentity ) $ chsRulesFromGraph chsActionId chsGraph
  }
  where
    chsActionId = ActionId 2

chsRulesFromGraph :: ActionId
                  -> Map ModuleName (Set ModuleName)
                  -> FreshT Identity (Map ModuleName RuleId)
chsRulesFromGraph chsActionId chsGraph =
  mfix $ \ mods ->
    flip Map.traverseWithKey chsGraph \ chsMod chiDeps -> do
      let modPath = toFilePath chsMod <.> "chs"
      registerRule $
        Rule
          { dependencies = ProjectFile modPath
                         : [ RuleResult $ RuleResultRef { ruleId = depId, ruleResultIndex = 1 }
                           | chiDep <- Set.toList chiDeps
                           , let depId = mods Map.! chiDep
                           ]
          , results = [ AutogenFile $ toFilePath chsMod <.> "hs"
                      , BuildFile $ toFilePath chsMod <.> "chi"
                      ]
          , actionId = chsActionId
          }







-----------------------------------
-- Stub functions for the examples.

-- Example 1: ex-nihilo module generation (stack).
writeStackBuildFile :: PreBuildComponentInputs -> BigIO ()
writeStackBuildFile _ = BigIO $
  putStrLn "Stub: I will write Build_Stack.hs"

stackBuildModule :: PreBuildComponentInputs -> ModuleName
stackBuildModule _ = ModuleName "Build_Stack"
  -- In practice this is something like: "Build_" ++ exeName exe

-- Example 2: c2hs preprocessing.

-- A call to @chs -M@.
callChsForDeps :: PreBuildComponentInputs -> SmallIO (Map ModuleName (Set ModuleName))
callChsForDeps _ = SmallIO $ do
  putStrLn "Stub call to 'c2hs -M'" -- (not that that exists)
  return $
    Map.fromList
      [ ( ModuleName "A", Set.empty )
      , ( ModuleName "B", Set.fromList [ ModuleName "A" ] )
      ]

runChs :: PreBuildComponentInputs
       -> ResolvedDependency -- ^ input @.chs@ file
       -> BigIO ()
runChs _buildInfoStuff (_baseDir, _chsPath) = BigIO $
  putStrLn $ unlines
    [ "Stub call to 'c2hs' executable."
    -- The file on which to call "c2hs" is: baseDir </> chsPath <.> "chs"
    -- The output dir is something like componentBuildDir (modulo -tmp weirdness)
    -- Add include directories: the componentBuildDir
    ]
