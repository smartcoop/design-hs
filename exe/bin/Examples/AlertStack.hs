module Examples.AlertStack
  ( alertStacks
  ) where

import           Smart.Html.AlertStack
import qualified Smart.Html.Shared.Html.Icons  as Icons

alertStacks :: [AlertStack]
alertStacks =
  [ AlertStack [Alert iconDone "This is a single alert!" NoButton]
  , AlertStack
    [ Alert iconDone "This is alert 0 of n" CloseButton
    , Alert iconDone "This is alert 1 of n" CloseButton
    , Alert iconDone "This is alert 2 of n" CloseButton
    ]
  ]
  where iconDone = Just Icons.svgIconCircleCheck
