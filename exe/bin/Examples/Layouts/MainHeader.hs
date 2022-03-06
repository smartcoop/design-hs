module Examples.Layouts.MainHeader
  ( mainHeader
  ) where

import           Smart.Html.Dsl
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A
import Examples.Navbar (exampleNavbar)

mainHeader :: HtmlCanvas
mainHeader = SingletonCanvas . div' $ H.toMarkup exampleNavbar
  where div' = H.div ! A.class_ "c-app-layout"
