module Examples.GlobalBanner
  ( globalBanners
  ) where

import           Smart.Html.GlobalBanner
import qualified Smart.Html.Shared.Html.Icons  as Icons

globalBanners :: [GlobalBanner]
globalBanners =
  [ GlobalBannerDefault (Just Icons.svgIconCircleCheck) "Well done!" CloseButton
  , GlobalBannerWarning (Just Icons.svgIconDelete) "Uh oh..." CloseButton
  ]
