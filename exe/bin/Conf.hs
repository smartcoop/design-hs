module Conf
  ( scaffoldFilesystem
  ) where

import           Conf.Types
import qualified System.Directory              as Dir
import           System.FilePath.Posix          ( (</>) )

-- | Set up the directory tree.
scaffoldFilesystem
  :: MonadIO m
  => Conf -- ^ System configuration 
  -> [FilePath] -- ^ Additional sub. directories to create 
  -> m ()
scaffoldFilesystem Conf {..} subdirs = liftIO $ createParent >> createChildren
 where
  createParent = Dir.createDirectoryIfMissing True _cOutputDir
  createChildren =
    let childSubdirs = (_cOutputDir </>) <$> subdirs
    in  mapM_ (Dir.createDirectoryIfMissing True) childSubdirs
