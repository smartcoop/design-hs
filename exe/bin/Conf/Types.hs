module Conf.Types
  ( Conf(..)
  ) where

-- | Run configuration
newtype Conf = Conf { _cOutputDir :: FilePath }
             deriving Show
