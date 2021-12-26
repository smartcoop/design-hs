{-# LANGUAGE DataKinds #-}
module Examples.IconList
  ( iconLists
  ) where

import           Smart.Html.IconList
import qualified Smart.Html.Shared.Html.Icons  as Icons

iconLists :: [IconList]
iconLists =
  [ IconList
      [ IconListElem (Icons.OSvgIconDiv @"bills" Icons.svgIconBills)
                     "Here are your bills"
      , IconListElem (Icons.OSvgIconDiv @"add" Icons.svgIconAdd)
                     "Add pending bills"
      , IconListElem (Icons.OSvgIconDiv @"email" Icons.svgIconEmail)
                     "contact@bills.com"
      , IconListElem (Icons.OSvgIconDiv @"phone" Icons.svgIconPhone)
                     "+32 488 40 5001"
      ]
  ]
