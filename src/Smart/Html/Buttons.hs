-- https://design.smart.coop/development/docs/c-button.html
{-# LANGUAGE OverloadedStrings #-}

module Smart.Html.Buttons where

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
buttonBackSecondary =
  H.button ! A.class_ "c-button c-button--secondary" ! A.type_ "button" $
    H.span ! A.class_ "c-button__content" $ do
      H.div ! A.class_ "o-svg-icon o-svg-icon-arrow-left  " $
        svgIconArrowLeft
      H.span ! A.class_ "c-button__label" $ "Back"

buttonNext =
  H.button ! A.class_ "c-button c-button--primary" ! A.type_ "button" $
    H.span ! A.class_ "c-button__content" $ do
      H.span ! A.class_ "c-button__label" $ "Next"
      H.div ! A.class_ "o-svg-icon o-svg-icon-arrow-right  " $
        svgIconArrowRight

buttonAdd :: String -> Html
buttonAdd label =
  H.button ! A.class_ "c-button c-button--primary" ! A.type_ "button" $
    H.span ! A.class_ "c-button__content" $ do
      H.div ! A.class_ "o-svg-icon o-svg-icon-add  " $
        svgIconAdd
      H.span ! A.class_ "c-button__label" $ H.toHtml label

buttonAddSecondary :: String -> Html
buttonAddSecondary label =
  H.button ! A.class_ "c-button c-button--secondary" ! A.type_ "button" $
    H.span ! A.class_ "c-button__content" $ do
      H.div ! A.class_ "o-svg-icon o-svg-icon-add" $
        svgIconAdd
      H.span ! A.class_ "c-button__label" $ H.toHtml label

buttonSave :: String -> Html
buttonSave label =
  H.button ! A.class_ "c-button c-button--primary" ! A.type_ "button" $ do
    H.span ! A.class_ "c-button__content" $ do
      H.div ! A.class_ "o-svg-icon o-svg-icon-save" $ do
        svgIconSave
      H.span ! A.class_ "c-button__label" $ H.toHtml label

buttonClodeDangerSecondary :: String -> Html
buttonClodeDangerSecondary label =
  H.button ! A.class_ "c-button c-button--danger-secondary" ! A.type_ "button" $ do
    H.span ! A.class_ "c-button__content" $ do
      H.div ! A.class_ "o-svg-icon o-svg-icon-close" $ do
        svgIconClose
      H.span ! A.class_ "c-button__label" $ H.toHtml label
