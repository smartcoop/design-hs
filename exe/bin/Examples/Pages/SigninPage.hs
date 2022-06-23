module Examples.Pages.SigninPage
  ( signinPage
  ) where

import           Smart.Html.Dsl                 ( HtmlCanvas )
import qualified Smart.Html.Dsl                as Dsl
import           Smart.Html.Shared.Html.Icons
import           Text.Blaze                     ( customAttribute )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


signinPage :: HtmlCanvas
signinPage = Dsl.SingletonCanvas $ do
  H.header
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container"
    $ H.div
    ! A.class_ "c-brand c-brand--small"
    $ H.a
    ! A.href "/"
    $ H.img
    ! A.src "https://design.smart.coop/images/logo.svg"
    ! A.alt "Smart"
  H.main
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container o-container--small"
    $ do
        H.div
          ! A.class_ "c-panel"
          $ H.div
          ! A.class_ "c-panel__body"
          $ H.form
          $ do
              H.h3 ! A.class_ "c-h2" $ "Sign in"
              H.div
                ! A.class_ "o-form-group-layout o-form-group-layout--standard"
                $ do
                    H.div ! A.class_ "o-form-group" $ do
                      H.label ! A.class_ "o-form-group__label" $ "Email"
                      H.div
                        ! A.class_ "o-form-group__controls"
                        $ H.input
                        ! A.class_ "c-input"
                        ! A.type_ "email"
                    H.div ! A.class_ "o-form-group" $ do
                      H.label
                        ! A.class_ "o-form-group__label"
                        ! A.for "passwordId"
                        $ "Password"
                      H.div
                        ! A.class_ "o-form-group__controls"
                        $ H.div
                        ! A.class_ "c-input-with-icon"
                        $ do
                            H.input
                              ! A.class_ "c-input"
                              ! A.id "passwordId"
                              ! A.type_ "password"
                            H.button
                              ! A.class_ "c-input-with-icon__toggle"
                              ! customAttribute "data-password-toggle" "1"
                              $ do
                                  H.div
                                    ! A.class_ "o-svg-icon o-svg-icon-eye"
                                    $ H.toMarkup svgIconEye
                                  H.div
                                    ! A.class_ "o-svg-icon o-svg-icon-eye-off"
                                    $ H.toMarkup svgIconEyeOff
                    H.div
                      ! A.class_ "o-form-group"
                      $ H.a
                      ! A.class_ "c-button c-button--primary c-button--block"
                      ! A.href "#"
                      $ H.div
                      ! A.class_ "c-button__content"
                      $ H.div
                      ! A.class_ "c-button__label"
                      $ "Sign in"
                    H.p ! A.class_ "c-or" $ H.span "or"
                    H.div
                      ! A.class_ "o-form-group"
                      $ H.a
                      ! A.class_ "c-button c-button--secondary c-button--block"
                      ! A.href "#"
                      $ H.div
                      ! A.class_ "c-button__content"
                      $ H.div
                      ! A.class_ "c-button__label"
                      $ "Register your account"
                    H.div ! A.class_ "o-form-group u-ta-center" $ ""
        H.div
          ! A.class_ "c-content u-text-center u-spacer-top-l"
          $ H.a
          ! A.class_ "u-text-muted"
          ! A.href "#"
          $ "Forgot password"

  H.footer
    ! A.id "footer"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container"
    $ do
        H.hr ! A.class_ "c-hr"
        H.div ! A.class_ "c-toolbar c-toolbar--auto" $ do
          H.div
            ! A.class_ "c-toolbar__left"
            $ H.div
            ! A.class_ "c-toolbar__item"
            $ H.div
            ! A.class_ "o-flex o-flex--vertical-center"
            $ do
                H.span
                  ! A.class_ "u-spacer-right-s"
                  $ "Don't have an account yet?"
                H.a ! A.class_ "u-text-muted" ! A.href "" $ "Sign up"
          H.div
            ! A.class_ "c-toolbar__right"
            $ H.ul
            ! A.class_
                "c-bordered-list-horizontal c-bordered-list-horizontal--muted"
            $ do
                H.li
                  $ H.a
                  ! A.class_ "u-text-muted"
                  ! A.href "#"
                  $ "Footer link"
                H.li
                  $ H.a
                  ! A.class_ "u-text-muted"
                  ! A.href "#"
                  $ "Footer link"
