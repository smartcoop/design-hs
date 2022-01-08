{-|
Module: Smart.Html.Render
Description: Functions to render a Canvas to Html or Text.

-}
module Smart.Html.Render
  ( renderCanvas
  , renderCanvasText
  , renderCanvasWithHead
  , renderCanvasWithHeadText
  , smartDesignHead
  ) where

import qualified Data.Text                     as T
import qualified Smart.Html.Dsl                as Dsl
import qualified Text.Blaze.Html.Renderer.Pretty
                                               as R
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | Render a Smart canvas as a complete HTML document.
renderCanvasText = T.pack . R.renderHtml . renderCanvas

-- | Render a Smart canvas as a complete HTML document, as Text.
renderCanvas canvas = do
  H.docType
  H.html
    ! A.class_ "u-maximize-height"
    ! A.dir "ltr"
    ! A.lang "en" $ renderCanvasWithHead canvas

-- | Render a Smart canvas ensuring the CSS etc. are properly imported, as Text.
renderCanvasWithHeadText :: Dsl.HtmlCanvas -> T.Text
renderCanvasWithHeadText = T.pack . R.renderHtml . renderCanvasWithHead

-- | Render a Smart canvas ensuring the CSS etc. are properly imported.
renderCanvasWithHead :: Dsl.HtmlCanvas -> H.Html
renderCanvasWithHead canvas = smartDesignHead >> body
  where body = H.body (H.toMarkup canvas) ! A.class_ "u-maximize-height"

-- | Markup for the smart CSS head etc.
smartDesignHead :: H.Html
smartDesignHead =
  H.head $ charset >> viewport >> maincss >> protocss >> custscss
 where
  charset = H.meta ! A.charset "utf-8"
  viewport =
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  maincss  = stylesheet "main.css"
  protocss = stylesheet "prototype.css"
  custscss = stylesheet "styleguide-customizations.css"
  stylesheet file = H.link ! A.rel "stylesheet" ! A.href
    ("https://design.smart.coop/css/" <> file)
