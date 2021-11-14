{- |
Module: Smart.Html.Ruler
Description: Rulers (only horizontal for now)

<https://design.smart.coop/development/docs/c-hr.html Docs & examples> 

-}
module Smart.Html.Ruler
  ( Ruler(..)
  ) where

import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

data Ruler = HorizontalRuler

instance H.ToMarkup Ruler where
  toMarkup HorizontalRuler = H.hr ! A.class_ "c-hr"
