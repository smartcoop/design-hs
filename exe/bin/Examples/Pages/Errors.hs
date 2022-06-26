module Examples.Pages.Errors
  ( notFound
  , notFoundWebsite
  ) where

import           Smart.Html.Dsl
import           Smart.Html.Errors
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A
import Examples.Navbar (exampleNavbar, exampleNavbarWebsite)

notFound :: HtmlCanvas
notFound = SingletonCanvas . div' $
  H.toMarkup exampleNavbar >> main' (H.toMarkup NotFound)
 where
  div' = H.div ! A.class_ "c-app-layout"
  main' = H.main ! A.class_ "u-maximize-width u-scroll-wrapper"

notFoundWebsite :: HtmlCanvas
notFoundWebsite = SingletonCanvas $
  H.toMarkup exampleNavbarWebsite >> main' (H.toMarkup NotFound)
 where
  main' = H.main ! A.class_ "u-maximize-width"

