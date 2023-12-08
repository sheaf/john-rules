
-- |
-- Module: Monitor
--
-- Functionality for monitoring files or directories, inspired by
-- @cabal-install@'s @Distribution.Client.FileMonitor@ module.
module Monitor
  ( MonitorFileOrDir(..)
  , MonitorKindFile(..)
  ) where

-- base
import GHC.Generics
  ( Generic )

-- binary
import Data.Binary
  ( Binary )

--------------------------------------------------------------------------------

data MonitorFileOrDir file dir
  = MonitorFile
      { monitorKindFile :: !MonitorKindFile
      , monitorPath :: !file
      }
  | MonitorDirContents
      { monitorDirLocation :: !dir
      }
  deriving stock ( Generic, Eq, Show )
  deriving anyclass Binary

data MonitorKindFile
  = FileExists
  | FileModTime
  | FileHashed
  | FileNotExists
  deriving stock ( Generic, Eq, Show )
  deriving anyclass Binary
