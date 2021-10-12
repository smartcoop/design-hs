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

import Smart.Html.Application

--------------------------------------------------------------------------------
toolsNewContract :: Html
toolsNewContract = document "Smart design system" $ do
  navbar exampleTree
  mainContent titlebar panels
