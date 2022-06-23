module Examples.Pages.LandingPage
  ( landingPage
  , navigation
  ) where

import           Smart.Html.Dsl                 ( HtmlCanvas )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Misc               as Misc
import           Smart.Html.Navbar
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


landingPage :: HtmlCanvas
landingPage = Dsl.SingletonCanvas $ do
  H.toMarkup navigation
  landing
  Misc.landingFooter "https://github.com/smartcoop/design-hs"

navigation :: NavbarWebsite
navigation = NavbarWebsite
  [ Entry "Components" (Link "/components/")
  , Entry "Pages"      (Link "/pages/")
  ]

landing = H.main ! A.class_ "o-container o-container--flex" $ do
  Misc.landingHero "Smart's design system, implemented in Haskell" $ H.p $ do
    "This site presents a Haskell implementation of the "
    H.a ! A.href "http://design.smart.coop" $ "Smart design system"
    "."
  H.div ! A.class_ "o-grid" $ do
    Misc.landingPanel "Open source" $ H.p $ do
      "Just like the reference implementation, this project is open source "
      "and available "
      H.a ! A.href "https://github.com/smartcoop/design-hs" $ "on GitHub"
      "."
    Misc.landingPanel "Experimental" $ H.p $ do
      "Haskell is not used in production at Smart. Instead an experiment "
      "is in progress to see if using it to write prototypes is useful."
