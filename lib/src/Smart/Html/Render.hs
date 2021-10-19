module Smart.Html.Render
  ( renderCanvasWithHead
  , smartDesignHead
  ) where

import qualified Smart.Html.Dsl                as Dsl
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

-- | Render a Smart canvas ensuring the CSS etc. are properly imported.
renderCanvasWithHead :: Dsl.Canvas H.ToMarkup -> H.Html
renderCanvasWithHead canvas = smartDesignHead >> H.body (H.toMarkup canvas)

-- | Markup for the smart CSS head etc.
smartDesignHead :: H.Html
smartDesignHead =
  H.head
    $   H.link
    H.! (A.rel "stylesheet")
    H.! (A.href "https://design.smart.coop/css/main.css")

