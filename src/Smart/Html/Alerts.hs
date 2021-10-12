-- https://design.smart.coop/development/docs/c-alert.html
{-# LANGUAGE OverloadedStrings #-}

module Smart.Html.Alerts where

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
alertTitleAndContent :: String -> Html -> Html
alertTitleAndContent title content =
  H.div ! A.class_ "c-alert c-alert--default" $ do
    H.div ! A.class_ "o-svg-icon o-svg-icon-circle-information  " $
      svgIconCircleInformation
    H.div ! A.class_ "c-alert__body" $
      H.div ! A.class_ "c-alert__text" $ do
        H.h4 ! A.class_ "c-alert__title" $ H.toHtml title
        H.div ! A.class_ "c-alert__message" $
          H.div ! A.class_ "c-content" $
            content
