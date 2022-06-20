module Examples.Pages.MainHeader
  ( mainHeader
  , mainHeaderWebsite
  ) where

import           Smart.Html.Dsl
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A
import Examples.Navbar (exampleNavbar, exampleNavbarWebsite)

-- | The main navigation bar, as used in an application page.
mainHeader :: HtmlCanvas
mainHeader = SingletonCanvas . div' $ H.toMarkup exampleNavbar
  where div' = H.div ! A.class_ "c-app-layout"

-- | The main navigation bar, as used in a website page.
mainHeaderWebsite :: HtmlCanvas
mainHeaderWebsite = SingletonCanvas $ H.toMarkup exampleNavbarWebsite
