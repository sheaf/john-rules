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
  ( (<.>) )

-- transformers
import Control.Monad.Trans.Class
  ( MonadTrans(..) )

-- john-rules
import API
import CabalStubs
  ( ModuleName(..), PreBuildComponentInputs
  , toFilePath
  )
import Rules

--------------------------------------------------------------------------------
-- Example usage of the API.

-- Example 1: generating modules ex-nihilo (e.g. stack)

stackRules :: PreBuildRules
stackRules = fromRulesM $ RulesM \ buildInfoStuff -> do
  let
    files = case buildInfoStuff of
      _ -> [ ( AutogenFile, "Build_Stack.hs")
           , ( AutogenFile, "Other/Stuff.hs")
           ]
  genBuildModActionId <- registerAction $
    Action $ \ _ ( ResultDirs resDir ) ->
      for_ files \ ( fileTy, modNm ) ->
        writeModuleFile buildInfoStuff ( resDir fileTy, modNm )
  return $ void $ registerRule $
    Rule
      { dependencies = []
      , actionId = genBuildModActionId
      , results = files
      }

-- Example 2: preprocessing using c2hs.

chsRules :: PreBuildRules
chsRules = fromRulesM $ RulesM \ buildInfoStuff -> do
  chsActionId <- registerAction $
    Action $ \ (inputChsLoc:_inputChiLocs) resDirs ->
        runChs buildInfoStuff inputChsLoc resDirs
  return $ do
    chsGraph <- lift $ callChsForDeps buildInfoStuff
    hoist ( pure . runIdentity ) $ chsRulesFromGraph chsActionId chsGraph

chsRulesFromGraph :: ActionId
                  -> Map ModuleName (Set ModuleName)
                  -> FreshT Rule RuleId Identity ()
chsRulesFromGraph chsActionId chsGraph =
--mfix $ \ mods ->
    for_ ( Map.toList chsGraph ) \ (chsMod, chiDeps) -> do
      let modPath = toFilePath chsMod <.> "chs"
      registerRule $
        Rule
          { dependencies = ProjectFile modPath
                         : [ ProjectFile $ toFilePath chiDep <.> "chi"
                           | chiDep <- Set.toList chiDeps
                        -- , let depId = mods Map.! chiDep
                           ]
          , results = [ (AutogenFile, toFilePath chsMod <.> "hs")
                      , (BuildFile, toFilePath chsMod <.> "chi")
                      ]
          , actionId = chsActionId
          }

-----------------------------------
-- Stub functions for the examples.

-- Example 1: ex-nihilo module generation (stack).
writeModuleFile :: PreBuildComponentInputs -> ResolvedLocation -> BigIO ()
writeModuleFile _cabalBuildInfoStuff (modDir, modNm) = BigIO $
  putStrLn $ "Stub: I will write " ++ modNm ++ " into directory " ++ modDir

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
       -> ResultDirs -- ^ directories in which to put outputs
       -> BigIO ()
runChs _buildInfoStuff _ _ = BigIO $
  putStrLn $ unlines
    [ "Stub call to 'c2hs' executable."
    -- The file on which to call "c2hs" is: baseDir </> chsPath <.> "chs"
    -- The output dir is something like componentBuildDir (modulo -tmp weirdness)
    -- Add include directories: the componentBuildDir
    ]
