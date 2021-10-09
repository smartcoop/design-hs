{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (find)
import qualified Data.Text.Lazy.IO as T
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (preEscapedToHtml, toHtml, (!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.Environment (getArgs)


--------------------------------------------------------------------------------
main :: IO ()
main = do
  T.putStr (renderHtml page)


--------------------------------------------------------------------------------
page :: Html
page = document $
  return ()


--------------------------------------------------------------------------------
document body = do
  H.docType
  H.html
    ! A.class_ "u-maximize-height"
    ! A.dir "ltr"
    ! A.lang "en" $ do
    body
