-- https://design.smart.coop/development/docs/c-global-banner.html
{-# LANGUAGE OverloadedStrings #-}

module Smart.Html.Banners where

import Control.Monad (forM_)
import Text.Blaze (customAttribute)
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (preEscapedToHtml, toHtml, (!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Svg as S (toSvg)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as SA

import Smart.Html.Icons


--------------------------------------------------------------------------------
banner =
  H.div ! A.class_ "c-global-banner c-global-banner--default" $ do
    H.div ! A.class_ "o-svg-icon o-svg-icon-circle-information o-svg-icon--default " $
      svgIconCircleInformation
    H.div ! A.class_ "c-global-banner__label" $
      H.p "Nam eget hendrerit massa, a consequat turpis."
    H.button ! A.class_ "c-button c-button--borderless c-button--icon"
             ! A.type_ "button"
             ! customAttribute "data-banner-close" "data-banner-close" $
      H.span ! A.class_ "c-button__content" $ do
        H.div ! A.class_ "o-svg-icon o-svg-icon-close  " $
          svgIconClose
        H.div ! A.class_ "u-sr-accessible" $ "Close"
