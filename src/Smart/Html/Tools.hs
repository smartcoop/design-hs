{-# LANGUAGE OverloadedStrings #-}
module Smart.Html.Tools where

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

import Smart.Html.Application hiding (subform1)
import Smart.Html.Icons

idSelectFunction :: String
idSelectFunction = "select-function"

--------------------------------------------------------------------------------
toolsNewContract :: Html
toolsNewContract = document "Smart design system - New contract" $ do
  navbar exampleTree
  mainContent (titlebar "New contract") $ do
    form
    dialogFullscreen idSelectFunction (dialogContent idSelectFunction)

form =
  vertically $
    mapM_ (uncurry panel)
      [ ("Contract type", subform1)
      ]

subform1 =
  group $ do
    inputDialog "function" "Your function"

inputDialog :: String -> String -> Html
inputDialog name label =
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" ! A.for (H.toValue name) $
      H.toHtml label
    H.div ! A.class_ "c-input-group" $ do
      H.input ! A.class_ "c-input" ! A.type_ "text" ! A.id (H.toValue name)
        ! A.value "Webmaster"
        ! A.readonly "readonly"
      H.div ! A.class_ "c-input-group__append"
        ! customAttribute "data-dialog" (H.toValue idSelectFunction) $
        H.div ! A.class_ "o-svg-icon o-svg-icon-edit" $
          svgIconEdit
