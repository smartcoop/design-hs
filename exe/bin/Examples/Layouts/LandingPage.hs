module Examples.Layouts.LandingPage
  ( landingPage
  , navigation
  ) where

import           Smart.Html.Dsl                 ( HtmlCanvas )
import qualified Smart.Html.Dsl                as Dsl
import           Smart.Html.Navbar
import           Smart.Html.Shared.Html.Icons   ( svgIconGitHub )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


landingPage :: HtmlCanvas
landingPage = Dsl.SingletonCanvas $ do
  H.toMarkup navigation
  landing
  footer

navigation :: NavbarWebsite
navigation = NavbarWebsite
  [ Entry "Components" (Link "/components/")
  , Entry "Layouts"    (Link "/layouts/")
  , Entry "Old"        (Link "/old/")
  ]

landing = H.main ! A.class_ "o-container o-container--flex" $ do
  H.div
    ! A.class_ "c-design-system-home-hero u-padding-vertical-xl"
    $ H.div
    ! A.class_ "o-grid"
    $ do
        H.div
          ! A.class_ "o-grid-col-bp3-6"
          $ H.div
          ! A.class_ "o-flex o-flex o-flex--vertical-center"
          ! A.style "height: 100%;"
          $ H.div
          ! A.class_ "c-display"
          $ do
              H.h1 "Smart's design system, implemented in Haskell"
              H.p $ do
                "This site presents a Haskell implementation of the "
                H.a ! A.href "http://design.smart.coop" $ "Smart design system"
                "."
        H.div
          ! A.class_ "o-grid-col-bp3-6"
          $ H.div
          ! A.class_ "c-design-system-home-illustration"
          $ H.img
          ! A.src "https://design.smart.coop/images/illustration-form.png"
          ! A.alt ""
  H.div ! A.class_ "o-grid" $ do
    H.div
      ! A.class_ "o-grid-col-bp3-6"
      $ H.div
      ! A.class_ "c-panel u-spacer-bottom"
      $ H.div
      ! A.class_ "c-panel__body"
      $ H.div
      ! A.class_ "c-display"
      $ do
          H.h3 "Open source"
          H.p $ do
            "Just like the reference implementation, this project is open source "
            "and available "
            H.a ! A.href "https://github.com/smartcoop/design-hs" $ "on GitHub"
            "."
    H.div
      ! A.class_ "o-grid-col-bp3-6"
      $ H.div
      ! A.class_ "c-panel u-spacer-bottom"
      $ H.div
      ! A.class_ "c-panel__body"
      $ H.div
      ! A.class_ "c-display"
      $ do
          H.h3 "Experimental"
          H.p
            "Haskell is not used in production at Smart. Instead an experiment is in progress to see if using it to write prototypes is useful."

footer =
  H.footer
    ! A.id "footer"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container"
    $ do
        H.hr ! A.class_ "c-hr"
        H.ul
          ! A.class_
              "c-bordered-list-horizontal c-bordered-list-horizontal--muted"
          $ H.li
          $ H.a
          ! A.href "https://github.com/smartcoop/design-hs"
          $ H.div
          ! A.class_ "o-flex"
          $ do
              H.div
                ! A.class_ "u-spacer-right-s"
                $ H.div
                ! A.class_ "o-svg-icon o-svg-icon-github"
                $ H.toMarkup svgIconGitHub
              "Share feedback on GitHub"
