module Examples.Layouts.MainHeader
  ( mainHeader
  ) where

import           Smart.Html.Dsl
import           Smart.Html.Navbar
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

mainHeader :: HtmlCanvas
mainHeader = SingletonCanvas . div' $ H.toMarkup Navbar
  where div' = H.div ! A.class_ "c-app-layout"
