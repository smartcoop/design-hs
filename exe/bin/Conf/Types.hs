module Conf.Types
  ( FilesystemConf(..)
  , Conf(..)
  ) where

-- | Run configuration
data FilesystemConf = FilesystemConf
  { _fcOutputDir      :: FilePath
  , _fcExamplesSubdir :: FilePath
  }
  deriving Show

newtype Conf = Conf { _cFilesystemConf :: FilesystemConf }
             deriving Show
