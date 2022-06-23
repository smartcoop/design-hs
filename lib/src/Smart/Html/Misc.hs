-- | Miscellaneous fragments of code. They probably need to be improved and
-- moved to some other modules.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Smart.Html.Misc
  ( landingHero
  , landingPanel
  , landingFooter
  ) where

import           Smart.Html.Shared.Html.Icons
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

landingHero title content =
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
              H.h1 title
              content
        landingImage

landingPanel title content =
  H.div
    ! A.class_ "o-grid-col-bp3-6"
    $ H.div
    ! A.class_ "c-panel u-spacer-bottom"
    $ H.div
    ! A.class_ "c-panel__body"
    $ H.div
    ! A.class_ "c-display"
    $ do
        H.h3 title
        content

landingImage =
  H.div
    ! A.class_ "o-grid-col-bp3-6"
    $ H.div
    ! A.class_ "c-design-system-home-illustration"
    $ H.img
    ! A.src "https://design.smart.coop/images/illustration-form.png"
    ! A.alt ""

landingFooter githubLink =
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
          ! A.href githubLink
          $ H.div
          ! A.class_ "o-flex"
          $ do
              H.div
                ! A.class_ "u-spacer-right-s"
                $ H.div
                ! A.class_ "o-svg-icon o-svg-icon-github"
                $ H.toMarkup svgIconGitHub
              "Share feedback on GitHub"

