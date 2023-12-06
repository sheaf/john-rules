module CabalStubs where

-- binary
import Data.Binary
  ( Binary )

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
  ( unpack )

-------------------------------------------------
-- Stubs of Cabal datatypes to ease in prototyping

newtype Suffix = Suffix Text
  deriving stock Show
  deriving newtype (Eq, Ord)

newtype PreBuildComponentInputs = PBCI ()
  deriving newtype Binary
newtype ModuleName = ModuleName Text
  deriving stock (Eq, Ord)
  deriving newtype (Show, Binary)

toFilePath :: ModuleName -> FilePath
toFilePath (ModuleName m) = Text.unpack m
