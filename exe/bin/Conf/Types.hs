module Conf.Types
  ( Conf(..)
  ) where

-- | Run configuration
data Conf = Conf
  { _cOutputDir      :: FilePath
  , _cExamplesSubdir :: FilePath
  }
  deriving Show
