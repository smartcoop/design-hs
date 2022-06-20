{-# LANGUAGE DataKinds #-}
module Examples.Alert
  ( alerts
  ) where

import           Smart.Html.Alert
import           Smart.Html.Button
import qualified Smart.Html.Shared.Html.Icons  as Icons

alerts :: [Alert]
alerts =
  [ Alert AlertDefault iconInformation loremIpsum NoButton
  , Alert AlertError   iconError       loremIpsum NoButton
  , Alert AlertWarning iconWarning     loremIpsum NoButton
  , Alert AlertSuccess iconSuccess     loremIpsum NoButton
  , Alert AlertDefault iconInformation loremIpsum CloseButton
  , AlertLight AlertDefault iconInformation loremIpsum CloseButton
  ]
 where
  iconInformation = Just
    $ Icons.OSvgIconDiv @"circle-information" Icons.svgIconCircleInformation
  iconError = Just $ Icons.OSvgIconDiv @"circle-error" Icons.svgIconCircleError
  iconWarning = Just $ Icons.OSvgIconDiv @"warning" Icons.svgIconWarning
  iconSuccess =
    Just $ Icons.OSvgIconDiv @"circle-check" Icons.svgIconCircleCheck
  loremIpsum = "Lorem, ipsum dolor sit amet consectetur adipisicing elit."
