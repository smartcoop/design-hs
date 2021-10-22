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
  createParent = mkdir _cOutputDir
  -- for all child subdirectories, we want to create the ones
  -- additionally supplied by the user, and the example subdirectory.
  createChildren =
    let childSubdirs = (_cOutputDir </>) <$> (_cExamplesSubdir : subdirs)
    in  mapM_ mkdir childSubdirs
  mkdir = Dir.createDirectoryIfMissing True