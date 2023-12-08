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

-- binary
import qualified Data.Binary as Binary
  ( encode )

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
import Monitor
import Rules

--------------------------------------------------------------------------------
-- Example usage of the API.

-- Example 1: generating modules ex-nihilo (e.g. stack)

stackRules :: PreBuildRules
stackRules = fromRulesM $ RulesM \ buildInfoStuff -> do
  let
    files = case buildInfoStuff of
      _ -> [ ( SrcFile, "Build_Stack.hs" )
           , ( SrcFile, "Other/Stuff.hs" )
           ]
  genBuildModActionId <- registerAction $
    Action $ \ _ ( ResolvedLocations resDir ) ->
      for_ files \ ( fileTy, modNm ) ->
        writeModuleFile buildInfoStuff ( resDir fileTy, modNm )
  return $ void $ registerRule $
    Rule
      { unresolvedDependencies = []
      , monitoredFiles = []
      , monitoredValue =
          Just $ case buildInfoStuff of { _ -> Binary.encode 'x' }
            -- We want to monitor the projection out of 'PackageDescription'
            -- that we care about, so that this rule gets rerun when this
            -- changes.
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
          fileDeps = ProjectSearchDirFile SrcFile modPath
                   : [ ProjectSearchDirFile BuildFile $ toFilePath chiDep <.> "chi"
                     | chiDep <- Set.toList chiDeps
                  -- , let depId = mods Map.! chiDep
                     ]
      registerRule $
        Rule
          { unresolvedDependencies = fileDeps
          , monitoredFiles = [ MonitorDirContents SrcFile ]
             -- Monitor source files dirs, so that if a user adds a new .chs
             -- file we know to re-run the "chs -M" computation.
          , monitoredValue = Just (Binary.encode ())
          , results = [ (SrcFile, toFilePath chsMod <.> "hs")
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
       -> ResolvedLocations -- ^ directories in which to put outputs
       -> BigIO ()
runChs _buildInfoStuff _ _ = BigIO $
  putStrLn $ unlines
    [ "Stub call to 'c2hs' executable."
    -- The file on which to call "c2hs" is: baseDir </> chsPath <.> "chs"
    -- The output dir is something like componentBuildDir (modulo -tmp weirdness)
    -- Add include directories: the componentBuildDir
    ]
