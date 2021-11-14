{- |
Module: Smart.Html.GlobalBanner
Description: Global banners.

<https://design.smart.coop/development/docs/c-global-banner.html Docs & examples>

TODO: incorrect HTML generation!

-}
module Smart.Html.GlobalBanner
  ( GlobalBanner(..)
  , Btn.ButtonDef(..)
  ) where

import qualified Smart.Html.Button             as Btn
import           Smart.Html.Shared.Html.Icons
import           Smart.Html.Shared.Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

data GlobalBanner
  = GlobalBannerDefault (Maybe Icon) Title Btn.ButtonDef
  | GlobalBannerWarning (Maybe Icon) Title Btn.ButtonDef

instance H.ToMarkup GlobalBanner where
  toMarkup = \case
    GlobalBannerDefault mIcon title button ->
      mkWithClass "default" mIcon title button
    GlobalBannerWarning mIcon title button ->
      mkWithClass "warning" mIcon title button
   where
    mkWithClass extraClass mIcon title button =
      (H.div ! A.class_ class_) $ mIconM >> titleM >> buttonM
     where
      mIconM = maybe mempty H.toMarkup mIcon
      titleM =
        (H.div ! A.class_ "c-global-banner__label") . H.p $ H.toMarkup title
      buttonM = H.toMarkup button
      class_  = "c-global-banner c-global-banner--" <> extraClass
