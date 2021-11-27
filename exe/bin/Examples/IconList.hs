module Examples.IconList
  ( iconLists
  ) where

import           Smart.Html.IconList
import qualified Smart.Html.Shared.Html.Icons  as Icons

iconLists :: [IconList]
iconLists =
  [ IconList
      [ IconListElem Icons.svgIconBills "Here are your bills"
      , IconListElem Icons.svgIconAdd   "Add pending bills"
      ]
  ]
