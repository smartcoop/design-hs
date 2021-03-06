{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.Lazy.IO as T
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import System.Environment (getArgs)

import qualified Smart.Html.Application as App
import qualified Smart.Html.Tools as Tools
import qualified Smart.Html.Website as Web


--------------------------------------------------------------------------------
-- Example usage:
--   runghc -isrc/ bin/generate.hs --pretty app-form
--   runghc -isrc/ bin/generate.hs --pretty blog-post
main :: IO ()
main = do
  args_ <- getArgs
  let (render, args) = case args_ of
        "--pretty" : rest -> (putStr . Pretty.renderHtml, rest)
        _ -> (T.putStr . renderHtml, args_)

  case args of
    ["index"] -> render Web.index
    ["app-empty"] -> render App.empty
    ["app-toolbar"] -> render App.navToolbar
    ["app-titlebar"] -> render App.navTitlebar
    ["app-form"] -> render App.page
    ["app-form--banner"] -> render App.pageWithBanner
    ["app-form--wizard"] -> render App.pageWithWizard
    ["app-form--side-menu"] -> render App.pageWithSideMenu
    ["app-dialog"] -> render App.pageWithDialog
    ["datagrid"] -> render App.datagrid
    ["registration"] -> render App.registration
    ["web-empty"] -> render Web.empty
    ["blog-post"] -> render Web.page
    ["tools-new-contract"] -> render Tools.toolsNewContract
    [name] -> putStrLn $ "Unknown example page \"" ++ name ++ "\"."
    _ -> putStrLn "Missing argument: example name"
