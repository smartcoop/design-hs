module Conf.Types
  ( FilesystemConf(..)
  , Conf(..)
  , relativeToParentSubdir
  ) where

import           Control.Lens
import qualified Data.Text                     as T

-- | Run configuration
data FilesystemConf = FilesystemConf
  { _fcOutputDir      :: FilePath
  , _fcExamplesSubdir :: FilePath
  }
  deriving Show

newtype Conf = Conf { _cFilesystemConf :: FilesystemConf }
             deriving Show

{- | Find the path of the examples subdirectory relative to the parent output directory. 

Examples:

@

λ> relativeToParentSubdir "/foo/bar/baz" "/foo/bar/example.html"
"example.html"
*Examples.Accordion Conf.Types
λ> relativeToParentSubdir "/foo/bar/baz" "/foo/bar/car/example.html"
"car/example.html"

@
-}
relativeToParentSubdir
  :: FilePath -- ^ Parent path (absolute) 
  -> FilePath -- ^ Child path (absolute) 
  -> FilePath -- ^ The directory path from the Parent Path to the Child path.
relativeToParentSubdir parent subdir =
  let fcOutputDirPaths = T.splitOn "/" (T.pack parent)
      -- Paths of the subdirectory, indexed. 
      subdirPathsIdxd  = zip [0 ..] $ T.splitOn "/" (T.pack subdir)
      -- This is fairly simple: we iterate through the parts of the subdirectory paths, ensuring that the idx of that part matches with
      -- the same idx on the parent paths. 
      subdirRelative   = dropWhile sameIxParent subdirPathsIdxd
      -- Check if the parent at the same index is the same directory. 
      sameIxParent (idx, subdirPath) =
        (fcOutputDirPaths ^? ix idx) == Just subdirPath
  in  T.unpack $ T.intercalate "/" $ snd <$> subdirRelative

