module Conf.Types
  ( FilesystemConf(..)
  , Conf(..)
  , relativeToParentSubdir
  ) where

import           Control.Lens
import qualified Data.Text                     as T

{- | Run configuration: filesystem.

The configuration has 2 parts:

1. The root directory under which to place the index.html file. 

2. The examples subdirectory (non-absolute) under which to place examples. 

--

For example: if we have:

@
FilesystemConf
{ _fcOutputDir      = "/foo/bar"
}
@

We'll end up with the following tree:

@
foo
└── bar
    ├── components
    │   └── some-example0.html
    ├── layouts
    │   └── some-example0.html
    └── index.html
@

The examples subdirectory will be created if it doesn't exist yet; and existing files will be overwritten. 
-}
data FilesystemConf = FilesystemConf
  { _fcOutputDir      :: FilePath -- ^ The output directory: the index.html file is placed here. 
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

