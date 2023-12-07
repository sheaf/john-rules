{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Examples
--
-- This module implements common patterns found in Custom Setup scripts in
-- terms of the fine-grained rules defined in this package.
module Examples where

-- base
import Control.Monad
  ( void )
import Data.Foldable
  ( for_ )
import Data.Functor.Identity
  ( Identity(..) )

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( fromList, toList )
import Data.Set
  ( Set )
import qualified Data.Set as Set
  ( empty, fromList, toList)

-- filepath
import System.FilePath
  ( (<.>), (</>) )

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
  { actions = \ _ ->
      let
        genBuildModAction =
          Action $ \ _ (stackBuildLoc:_) -> writeStackBuildFile stackBuildLoc
      in Map.fromList [ ( genBuildModActionId, genBuildModAction ) ]
  , rules = \ buildInfoStuff ->
      void $ registerRule $
        Rule
          { dependencies = []
          , actionId = genBuildModActionId
          , results = [ AutogenFile $ toFilePath (stackBuildModule buildInfoStuff) ]
          }
  }
  where
    genBuildModActionId = ActionId 1

-- Example 2: preprocessing using c2hs.

chsRules :: PreBuildRules
chsRules = Rules
  { actions = \ buildInfoStuff ->
      let
        runChsAction =
          Action $ \ (inputChsLoc:_inputChiLocs) (outputHsLoc:outputChiLoc:_) ->
            runChs buildInfoStuff inputChsLoc outputHsLoc outputChiLoc
      in Map.fromList [ ( chsActionId, runChsAction ) ]
  , rules = \ buildInfoStuff -> do
      chsGraph <- liftFresh $ callChsForDeps buildInfoStuff
      hoist ( pure . runIdentity ) $ chsRulesFromGraph chsActionId chsGraph
  }
  where
    chsActionId = ActionId 2

chsRulesFromGraph :: ActionId
                  -> Map ModuleName (Set ModuleName)
                  -> FreshT Identity ()
chsRulesFromGraph chsActionId chsGraph =
--mfix $ \ mods ->
    for_ (Map.toList chsGraph) \ (chsMod, chiDeps) -> do
      let modPath = toFilePath chsMod <.> "chs"
      registerRule $
        Rule
          { dependencies = ProjectFile modPath
                         : [ ProjectFile $ toFilePath chiDep <.> "chi"
                           | chiDep <- Set.toList chiDeps
                        -- , let depId = mods Map.! chiDep
                           ]
          , results = [ AutogenFile $ toFilePath chsMod <.> "hs"
                      , BuildFile $ toFilePath chsMod <.> "chi"
                      ]
          , actionId = chsActionId
          }

-----------------------------------
-- Stub functions for the examples.

-- Example 1: ex-nihilo module generation (stack).
writeStackBuildFile :: ResolvedLocation -> BigIO ()
writeStackBuildFile (modDir, modNm) = BigIO $
  putStrLn $ "Stub: I will write Build_Stack.hs to " ++ (modDir </> modNm)

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
       -> ResolvedLocation -- ^ location of input @.chs@ file
       -> ResolvedLocation -- ^ desired location of output @.hs@ file
       -> ResolvedLocation -- ^ desired location of output @.chi@ file
       -> BigIO ()
runChs _buildInfoStuff _ _ _ = BigIO $
  putStrLn $ unlines
    [ "Stub call to 'c2hs' executable."
    -- The file on which to call "c2hs" is: baseDir </> chsPath <.> "chs"
    -- The output dir is something like componentBuildDir (modulo -tmp weirdness)
    -- Add include directories: the componentBuildDir
    ]
