{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.Lazy.IO as T
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import System.Environment (getArgs)

import qualified Smart.Html.Application as App


--------------------------------------------------------------------------------
-- Example usage:
--   runghc -i src/ bin/generate.hs --pretty app-form
main :: IO ()
main = do
  args_ <- getArgs
  let (render, args) = case args_ of
        "--pretty" : rest -> (putStr . Pretty.renderHtml, rest)
        _ -> (T.putStr . renderHtml, args_)

  case args of
    ["app-form"] -> render App.page
    [name] -> putStrLn $ "Unknown example page \"" ++ name ++ "\"."
    _ -> putStrLn "Missing argument: example name"
