module Smart.Html.Class
  () where

import qualified Text.Blaze.Html               as B

-- | A smart design element.
class Elem e where
  -- | Render/draw an element as HTML 
  renderElem :: e -> B.Html
