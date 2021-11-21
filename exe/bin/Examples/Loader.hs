module Examples.Loader
  ( loaders
  ) where

import           Smart.Html.Loader

loaders :: [Loader]
loaders = [ThreeBouncingDots (Just "MyDefaultLoader")]
