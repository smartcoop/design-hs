-- | Miscellaneous fragments of code. They probably need to be improved and
-- moved to some other modules.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Smart.Html.Misc
  ( landingHero
  , landingPanel
  , landingFooter
  , emptyPage
  , navToolbar
  , navTitlebar
  , page
  , pageWithBanner
  , pageWithWizard
  , pageWithSideMenu
  , pageWithDialog
  , datagrid
  , registration
  , toolsNewContract
  , webEmpty
  , webPage
  , js
  ) where

import           Smart.Html.Shared.Html.Icons
import           Text.Blaze                     ( customAttribute )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                , preEscapedToHtml
                                                , toHtml
                                                )
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Text.Blaze.Svg11              as S
import qualified Text.Blaze.Svg11.Attributes   as SA


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


--------------------------------------------------------------------------------
emptyPage :: Html
emptyPage = document "Smart design system" $ do
  return ()


--------------------------------------------------------------------------------
navToolbar :: Html
navToolbar = document "Smart design system" $ do
  mainContent toolbar (return ())


--------------------------------------------------------------------------------
navTitlebar :: Html
navTitlebar = document "Smart design system" $ do
  mainContent (titlebar "Module title") (return ())


--------------------------------------------------------------------------------
-- https://design.smart.coop/development/template-examples/app-form.html
page :: Html
page = document "Smart design system" $ do
  mainContent toolbar panels


--------------------------------------------------------------------------------
-- https://design.smart.coop/development/template-examples/global-banner.html
pageWithBanner :: Html
pageWithBanner = document "Smart design system" $ do
  banner
  mainContent toolbar panels


--------------------------------------------------------------------------------
-- https://design.smart.coop/development/template-examples/wizard.html
pageWithWizard :: Html
pageWithWizard = document "Smart design system" $ do
  mainContent wizard (panels' "Location and dates")


--------------------------------------------------------------------------------
-- https://design.smart.coop/development/template-examples/app-side-menu.html
pageWithSideMenu :: Html
pageWithSideMenu = document "Smart design system" $ do
  mainContentSideMenu menu toolbar panels


--------------------------------------------------------------------------------
-- https://design.smart.coop/development/template-examples/dialog.html
pageWithDialog :: Html
pageWithDialog = document "Smart design system" $ do
  mainContent toolbar
              (panels >> dialogVisible "dialog" (dialogContent "dialog"))


--------------------------------------------------------------------------------
--https://design.smart.coop/development/template-examples/app-datagrid.html
datagrid :: Html
datagrid = document "Smart design system" $ do
  mainContent (titlebar "Module title") table


--------------------------------------------------------------------------------
--https://design.smart.coop/development/template-examples/register-form.html
registration :: Html
registration = do
  let title = "Smart design system"
  H.docType
  H.html ! A.class_ "u-maximize-height" ! A.dir "ltr" ! A.lang "en" $ do
    myHead title
    H.body ! A.class_ "u-maximize-height" $ do
      H.div
        ! A.class_ "u-spacer-l"
        $ H.div
        ! A.class_ "o-container-vertical"
        $ H.div
        ! A.class_ "o-container o-container--medium"
        $ do
            H.div
              ! A.class_ "u-spacer-bottom"
              $ H.div
              ! A.class_ "c-brand c-brand--small"
              $ H.a
              ! A.href "/"
              $ H.img
              ! A.src "https://design.smart.coop/images/logo.svg"
              ! A.alt "Smart"
            H.div ! A.class_ "c-hr" $ ""
            H.h1 ! A.class_ "c-h2" $ "Register your account"
            H.div ! A.class_ "c-content" $ H.p $ do
              "For optimal use of the Smart services, it is highly recommended to attend a Smart session: view our "
              H.a ! A.href "#" $ "user manual"
              "."
            H.div ! A.class_ "u-spacer-bottom" $ do
              alertTitleAndContent "Important" $ H.ul $ do
                  -- TODO “Smart account”
                H.li
                  "You will receive your personal codes to access your \"Smart account\" (and to fill in your contracts) by email within 48 working hours of sending your registration documents. Do not hesitate to contact the nearest Smart office in case of emergency."
                H.li
                  "Remember to always declare your working days in advance. It is not allowed to backdate your services."

            H.div
              ! A.class_ "o-form-group-layout o-form-group-layout--standard"
              $ do
                  H.div ! A.class_ "o-grid" $ do
                    inputText6 "registerFormName"      "Name"
                    inputText6 "registerFormFirstName" "First name"
                    inputSelect6 "registerFormGender"
                                 "Gender"
                                 ["Male", "Female", "X"]
                    inputSelect6
                      "registerFormLangue"
                      "Language"
                      ["Francais", "Nederlands", "Deutsch", "Espanol"]
                      -- TODO Français

                  H.div ! A.class_ "c-hr" $ ""
                  H.h3 ! A.class_ "c-h3" $ "Official address"
                  H.div ! A.class_ "o-grid" $ do
                    inputText8 "registerFormAddressOfficalStreet" "Street"
                    inputText2 "registerFormAddressOfficalStreetNo" "Number"
                    inputText2
                      "registerFormAddressOfficalStreetBuildingAddition"
                      "Addition"
                    inputText6 "registerFormAddressOfficalZipCode" "Postal code"
                    inputSelect6 "registerFormAddressOfficalCountry"
                                 "Country"
                                 countries

                  H.div ! A.class_ "c-hr" $ ""
                  H.h3 ! A.class_ "c-h3" $ "Mailing address"
                  H.div ! A.class_ "o-grid" $ do
                    inputText8 "registerFormAddressPostalStreet" "Street"
                    inputText2 "registerFormAddressPostalStreetNo" "Number"
                    inputText2
                      "registerFormAddressPostalStreetBuildingAddition"
                      "Addition"
                    inputText6 "registerFormAddressPostalZipCode" "Postal code"
                    inputSelect6 "registerFormAddressPostalCountry"
                                 "Country"
                                 countries

                  H.div ! A.class_ "c-hr" $ ""
                  H.div ! A.class_ "o-grid" $ do
                    inputText6 "registerFormCellPhoneNumber" "Mobile number"
                    inputText6 "registerFormHomePhoneNumber" "Home phone number"
                    inputEmail "registerFormfieldEmail" "Email"
                    inputDate "registerFormfieldBirthdate" "Birthdate"
                    inputSelect6 "registerFormNativeCountry"
                                 "Native country"
                                 countries
                    inputSelect6
                      "registerFormfieldMaritalStatus"
                      "Marital status"
                      [ "Select"
                      , "Single"
                      , "Married"
                      , "Widowed"
                      , "Separated"
                      , "Divorced"
                      ]
                    inputText6 "registerFormfieldCityOfBirth" "City of birth"
                    inputText6 "registerFormfieldNationalRegisterNo"
                               "National register number"
                    inputText6 "registerFormfieldIdentityCardNumber"
                               "Identity card number"
                    inputText6 "registerFormfieldIbanNo" "IBAN number"
                    inputText6 "registerFormfieldProfessionalWithholdingTax"
                               "Professional withholding tax"
                    inputText6 "registerFormfieldMainFunction"  "Main function"
                    inputText6 "registerFormfieldOtherFunction" "Other function"

                  H.div ! A.class_ "o-form-group" $ do
                    H.div ! A.class_ "c-checkbox" $ do
                      H.label $ do
                        H.input ! A.type_ "checkbox" ! A.checked "checked"
                        H.div $ do
                          "I authorise Smart to transmit my data to third parties for professional purposes only."
                          H.a
                            ! A.class_ "o-flex o-flex--vertical-center"
                            ! A.href "#"
                            $ do
                                H.div
                                  ! A.class_ "u-spacer-right-s"
                                  $ "Read more here"
                                H.div
                                  ! A.class_
                                      "o-svg-icon o-svg-icon-external-link  "
                                  $ H.toMarkup
                                  $ svgIconExternalLink
                  H.div
                    ! A.class_ "o-form-group"
                    $ H.input
                    ! A.class_ "c-button c-button--primary c-button--block"
                    ! A.type_ "submit"
                    ! A.value "Continue"

    js


--------------------------------------------------------------------------------
inputText :: Text -> Text -> Html
inputText name label = H.div ! A.class_ "o-form-group" $ do
  H.label ! A.class_ "o-form-group__label" ! A.for (H.toValue name) $ H.toHtml
    label
  H.div
    ! A.class_ "o-form-group__controls"
    $ H.input
    ! A.class_ "c-input"
    ! A.type_ "text"
    ! A.id (H.toValue name)

inputText2 :: Text -> Text -> Html
inputText2 name label = H.div ! A.class_ "o-grid-col-2" $ inputText name label

inputText6 :: Text -> Text -> Html
inputText6 name label = H.div ! A.class_ "o-grid-col-6" $ inputText name label

inputText8 :: Text -> Text -> Html
inputText8 name label = H.div ! A.class_ "o-grid-col-8" $ inputText name label

inputEmail :: Text -> Text -> Html
inputEmail name label =
  H.div ! A.class_ "o-grid-col-6" $ H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" ! A.for (H.toValue name) $ H.toHtml
      label
    H.div
      ! A.class_ "o-form-group__controls"
      $ H.input
      ! A.class_ "c-input"
      ! A.type_ "email"
      ! A.id (H.toValue name)

inputDate :: Text -> Text -> Html
inputDate name label =
  H.div ! A.class_ "o-grid-col-6" $ H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" ! A.for (H.toValue name) $ H.toHtml
      label
    H.div
      ! A.class_ "o-form-group__controls"
      $ H.input
      ! A.class_ "c-input"
      ! A.type_ "date"
      ! A.id (H.toValue name)

inputSelect_ :: Text -> Text -> [Text] -> Maybe Text -> Html
inputSelect_ name label values mhelp = H.div ! A.class_ "o-form-group" $ do
  H.label ! A.class_ "o-form-group__label" ! A.for (H.toValue name) $ H.toHtml
    label
  H.div ! A.class_ "o-form-group__controls" $ do
    H.div
      ! A.class_ "c-select-holder"
      $ H.select
      ! A.class_ "c-select"
      ! A.id (H.toValue name)
      $ mapM_ (H.option . H.toHtml) values
    maybe (return ())
          (\s -> H.p ! A.class_ "c-form-help-text" $ H.toHtml s)
          mhelp

inputSelect :: Text -> Text -> [Text] -> Html
inputSelect name label values = inputSelect_ name label values Nothing

inputSelect' :: Text -> Text -> [Text] -> Text -> Html
inputSelect' name label values help =
  inputSelect_ name label values (Just help)

inputSelect6 :: Text -> Text -> [Text] -> Html
inputSelect6 name label values = H.div ! A.class_ "o-grid-col-6" $ do
  inputSelect name label values

inputTextarea :: Text -> Text -> Int -> Text -> Html
inputTextarea name label rows help = H.div ! A.class_ "o-form-group" $ do
  H.label ! A.class_ "o-form-group__label" ! A.for (H.toValue name) $ H.toHtml
    label
  H.div ! A.class_ "o-form-group__controls" $ do
    H.textarea
      ! A.class_ "c-textarea"
      ! A.rows (H.toValue $ show @Int @Text rows)
      ! A.id (H.toValue name)
      $ ""
    H.p ! A.class_ "c-form-help-text" $ H.toHtml help

inputRadios :: Text -> Text -> [(Text, Bool)] -> Html
inputRadios name label labels = H.div ! A.class_ "o-form-group" $ do
  H.label ! A.class_ "o-form-group__label" $ H.toHtml label
  H.div
    ! A.class_ "o-form-group__controls"
    $ H.div
    ! A.class_ "c-radio-group"
    $ do
        mapM_ radio labels
 where
  radio (label, checked) = H.div ! A.class_ "c-radio" $ H.label $ do
    if checked
      then H.input ! A.type_ "radio" ! A.name (H.toValue name) ! A.checked
        "checked"
      else H.input ! A.type_ "radio" ! A.name (H.toValue name)
    H.toHtml label


--------------------------------------------------------------------------------
mainContent top content =
  H.main ! A.class_ "u-scroll-wrapper u-maximize-width" $ do
    top
    H.div ! A.class_ "u-scroll-wrapper-body" $ content

mainContentSideMenu menu top content =
  H.main ! A.class_ "u-scroll-wrapper u-maximize-width" $ do
    H.div ! A.class_ "c-app-layout-inner" $ do
      H.div ! A.class_ "c-app-layout-inner__sidebar u-bg-gray-50" $ menu
      H.div
        ! A.class_ "c-app-layout-inner__main"
        $ H.div
        ! A.class_ "u-scroll-wrapper"
        $ do
            top
            H.div ! A.class_ "u-scroll-wrapper-body" $ content


--------------------------------------------------------------------------------
toolbar =
  H.div
    ! A.class_ "c-navbar c-navbar--bordered-bottom"
    $ H.div
    ! A.class_ "c-toolbar"
    $ do
        H.div ! A.class_ "c-toolbar__left" $ do
          H.div ! A.class_ "c-toolbar__item" $ do
            H.a
              ! A.class_ "c-button c-button--icon c-button--borderless"
              ! A.href "#"
              $ do
                  H.div ! A.class_ "c-button__content" $ do
                    H.div ! A.class_ "o-svg-icon o-svg-icon-arrow-left" $ do
                      H.toMarkup $ svgIconArrowLeft
                    H.div ! A.class_ "u-sr-accessible" $ "Back"
          H.div ! A.class_ "c-toolbar__item" $ do
            H.h2 ! A.class_ "c-toolbar__title" $ "Toolbar title"
        H.div ! A.class_ "c-toolbar__right" $ do
          H.div ! A.class_ "c-toolbar__item" $ do
            H.div ! A.class_ "c-button-toolbar" $ do
              buttonClodeDangerSecondary "Cancel"
              buttonSave "Save"

wizard =
  H.div
    ! A.class_ "c-navbar c-navbar--bordered-bottom"
    $ H.div
    ! A.class_ "c-toolbar"
    $ do
        H.div
          ! A.class_ "c-toolbar__left"
          $ H.div
          ! A.class_ "c-toolbar__item"
          $ H.nav
          ! A.class_ "c-wizard"
          $ H.ul
          $ do
              H.li
                $ H.a
                ! A.class_ "c-wizard__item c-wizard--complete"
                ! A.href "#"
                $ do
                    H.div ! A.class_ "c-wizard__indicator" $ ""
                    H.div ! A.class_ "c-wizard__label" $ "General info"
              H.li
                $ H.a
                ! A.class_ "c-wizard__item c-wizard--active"
                ! A.href "#"
                $ do
                    H.div ! A.class_ "c-wizard__indicator" $ "2"
                    H.div ! A.class_ "c-wizard__label" $ "Location and dates"
              H.li $ H.a ! A.class_ "c-wizard__item" ! A.href "#" $ do
                H.div ! A.class_ "c-wizard__indicator" $ "3"
                H.div ! A.class_ "c-wizard__label" $ "Function and risks"
              H.li $ H.a ! A.class_ "c-wizard__item" ! A.href "#" $ do
                H.div ! A.class_ "c-wizard__indicator" $ "4"
                H.div ! A.class_ "c-wizard__label" $ "Contract type"
              H.li $ H.a ! A.class_ "c-wizard__item" ! A.href "#" $ do
                H.div ! A.class_ "c-wizard__indicator" $ "5"
                H.div ! A.class_ "c-wizard__label" $ "Confirm"
        H.div
          ! A.class_ "c-toolbar__right"
          $ H.div
          ! A.class_ "c-toolbar__item"
          $ H.div
          ! A.class_ "c-button-toolbar"
          $ do
              buttonBackSecondary
              buttonNext

titlebar :: Text -> Html
titlebar title =
  H.div
    ! A.class_ "c-navbar c-navbar--bordered-bottom"
    $ H.div
    ! A.class_ "c-toolbar"
    $ do
        H.div
          ! A.class_ "c-toolbar__left"
          $ H.div
          ! A.class_ "c-toolbar__item"
          $ H.h2
          ! A.class_ "c-toolbar__title"
          $ H.toHtml title
        H.div
          ! A.class_ "c-toolbar__right"
          $ H.div
          ! A.class_ "c-toolbar__item"
          $ H.div
          ! A.class_ "c-button-toolbar"
          $ buttonAdd "Add item"


--------------------------------------------------------------------------------
menu = H.ul ! A.class_ "c-side-menu" $ do
  H.li
    ! A.class_ "c-side-menu__item c-side-menu__item--active"
    $ H.a
    ! A.class_ "c-side-menu__link"
    ! A.href "#"
    $ do
        H.div
          ! A.class_ "o-svg-icon o-svg-icon-document  "
          $ H.toMarkup
          $ svgIconDocument
        H.div ! A.class_ "c-sidebar-item__label" $ "Quotes & invoices"
  H.li
    ! A.class_ "c-side-menu__item"
    $ H.a
    ! A.class_ "c-side-menu__link"
    ! A.href "#"
    $ do
        H.div
          ! A.class_ "o-svg-icon o-svg-icon-bills  "
          $ H.toMarkup
          $ svgIconBills
        H.div ! A.class_ "c-sidebar-item__label" $ "Funding"
  H.li
    ! A.class_ "c-side-menu__item"
    $ H.a
    ! A.class_ "c-side-menu__link"
    ! A.href "#"
    $ do
        H.div ! A.class_ "o-svg-icon o-svg-icon-tag  " $ H.toMarkup $ svgIconTag
        H.div ! A.class_ "c-sidebar-item__label" $ "Expenses"


--------------------------------------------------------------------------------
panels = panels_ Nothing

panels' = panels_ . Just

panels_ mtitle = vertically $ do
  maybe (return ()) (\t -> H.div ! A.class_ "c-content" $ H.h1 t) mtitle
  mapM_ (panel "Form grouping") [subform1, subform2, subform3]

vertically content =
  H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ content

formGroup content =
  H.div ! A.class_ "o-form-group-layout o-form-group-layout--standard" $ content

groupHorizontal content =
  H.div
    ! A.class_ "o-form-group-layout o-form-group-layout--horizontal"
    $ content

panel :: Text -> Html -> Html
panel title content = H.div ! A.class_ "c-panel u-spacer-bottom-l" $ do
  H.div
    ! A.class_ "c-panel__header"
    $ H.h2
    ! A.class_ "c-panel__title"
    $ H.toHtml title
  H.div ! A.class_ "c-panel__body" $ do
    content

subform1 = formGroup $ do
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" $ "Add client"
    H.div ! A.class_ "c-empty-state c-empty-state--bg-alt" $ do
      H.p
        ! A.class_ "u-text-muted c-body-1"
        $ "Please add a client for this quote."
      H.div ! A.class_ "c-button-toolbar" $ do
        buttonAddSecondary "Add new client"
        buttonAddSecondary "Add existing client"
  inputRadios
    "radio1"
    "Radio"
    [ ("Lorem ipsum dolor sit amet.", True)
    , ("Lorem ipsum dolor sit amet.", False)
    ]

subform2 = formGroup $ do
  inputText "input" "Input"
  inputSelect'
    "select"
    "Select"
    ["Choose an item", "A", "B", "C"]
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed commodo accumsan risus."
  inputTextarea
    "textarea"
    "Textarea"
    5
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed commodo accumsan risus."

subform3 = formGroup $ do
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" ! A.for "input" $ "Nr BCE"
    H.div
      ! A.class_ "o-form-group__controls"
      $ H.input
      ! A.class_ "c-input"
      ! A.type_ "text"
      ! A.placeholder "__/__/__"
      ! A.id "input"
  H.div ! A.class_ "o-form-group" $ do
    H.label
      ! A.class_ "o-form-group__label"
      ! A.for "saleAmount"
      $ "Sale amount"
    H.div ! A.class_ "c-input-group" $ do
      H.input ! A.class_ "c-input" ! A.type_ "number" ! A.id "saleAmount"
      H.div ! A.class_ "c-input-group__append" $ "X"
      -- TODO Use "€" instead of "X"
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" ! A.for "time" $ "Time"
    H.div ! A.class_ "o-flex o-flex--vertical-center o-flex--spaced" $ do
      H.span $ do
        H.p "From"
        H.input ! A.class_ "c-input" ! A.type_ "time" ! A.id "time"
      H.span $ do
        H.p "To"
        H.input ! A.class_ "c-input" ! A.type_ "time" ! A.id "time"
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" ! A.for "date" $ "Default label"
    H.div
      ! A.class_ "o-form-group__controls"
      $ H.input
      ! A.class_ "c-input"
      ! A.type_ "date"
      ! A.id "date"


--------------------------------------------------------------------------------
table = H.div ! A.class_ "u-padding-horizontal-s" $ do
  H.table ! A.class_ "c-table c-table--styled js-data-table" $ do
    H.thead $ H.tr $ do
      H.th "Data"
      H.th "Data"
      H.th "Data"
      H.th ""
    H.tbody $ forM_ [1 .. 10] $ \_ -> H.tr $ do
      H.td "Data"
      H.td "Data"
      H.td "Data"
      H.td $ rowAction
  pagination

rowAction = H.div ! A.class_ "c-button-toolbar" $ do
  H.button
    ! A.class_ "c-button c-button--borderless c-button--icon"
    ! A.type_ "button"
    ! customAttribute "data-menu-placement" "bottom-end"
    ! customAttribute "data-menu"           "dropdownMenu-0"
    $ H.span
    ! A.class_ "c-button__content"
    $ do
        H.div
          ! A.class_ "o-svg-icon o-svg-icon-options-horizontal"
          $ H.toMarkup
          $ svgIconOptionsHorizontal
        H.div ! A.class_ "u-sr-accessible" $ "More options"
  H.ul ! A.class_ "c-menu" ! A.id "dropdownMenu-0" $ do
    H.li
      ! A.class_ "c-menu__item"
      $ H.a
      ! A.class_ "c-menu__label"
      ! A.href "#"
      $ do
          H.div
            ! A.class_ "o-svg-icon o-svg-icon-edit"
            $ H.toMarkup
            $ svgIconEdit
          H.span $ "Edit"
    H.li
      ! A.class_ "c-menu__item"
      $ H.a
      ! A.class_ "c-menu__label"
      ! A.href "#"
      $ do
          H.div
            ! A.class_ "o-svg-icon o-svg-icon-delete"
            $ H.toMarkup
            $ svgIconDelete
          H.span "Delete"
  H.button
    ! A.class_ "c-button c-button--borderless c-button--icon"
    ! A.type_ "button"
    $ H.span
    ! A.class_ "c-button__content"
    $ do
        H.div
          ! A.class_ "o-svg-icon o-svg-icon-chevron-right"
          $ H.toMarkup
          $ svgIconChevronRight
        H.div ! A.class_ "u-sr-accessible" $ "Go to detail"

pagination =
  H.div
    ! A.class_ "u-padding-horizontal"
    $ H.div
    ! A.class_ "u-spacer-top"
    $ H.div
    ! A.class_ "c-toolbar c-toolbar--auto"
    $ do
        H.div
          ! A.class_ "c-toolbar__left"
          $ H.div
          ! A.class_ "c-toolbar__item"
          $ H.div
          ! A.class_ "c-pagination-simple"
          $ do
              H.div ! A.class_ "c-pagination-simple__item" $ "1 of 4"
              H.div
                ! A.class_ "c-pagination-simple__item"
                $ H.div
                ! A.class_ "c-button-toolbar"
                $ do
                    H.button
                      ! A.class_ "c-button c-button--icon c-button--borderless"
                      ! A.disabled "disabled"
                      $ H.div
                      ! A.class_ "c-button__content"
                      $ do
                          H.div
                            ! A.class_ "o-svg-icon o-svg-icon-chevron-left"
                            $ H.toMarkup
                            $ svgIconChevronLeft
                          H.div ! A.class_ "u-sr-accessible" $ "Previous"
                    H.button
                      ! A.class_ "c-button c-button--icon c-button--borderless"
                      $ H.div
                      ! A.class_ "c-button__content"
                      $ do
                          H.div
                            ! A.class_ "o-svg-icon o-svg-icon-chevron-right"
                            $ H.toMarkup
                            $ svgIconChevronRight
                          H.div ! A.class_ "u-sr-accessible" $ "Next"
        H.div
          ! A.class_ "c-toolbar__right"
          $ H.div
          ! A.class_ "c-toolbar__item"
          $ H.div
          ! A.class_ "o-form-group-layout o-form-group-layout--horizontal"
          $ H.div
          ! A.class_ "o-form-group"
          $ do
              H.label
                ! A.class_ "u-spacer-right-s"
                ! A.for "itemsPerPageId"
                $ "Items per page"
              H.div
                ! A.class_ "o-form-group__controls"
                $ H.div
                ! A.class_ "c-select-holder"
                $ H.select
                ! A.class_ "c-select"
                ! A.id "itemsPerPageId"
                $ do
                    H.option "10"
                    H.option "20"
                    H.option "50"


--------------------------------------------------------------------------------
document title body = do
  H.docType
  H.html ! A.class_ "u-maximize-height" ! A.dir "ltr" ! A.lang "en" $ do
    myHead title
    myBody body


--------------------------------------------------------------------------------
myHead title = H.head $ do
  H.meta ! A.charset "utf-8"
  H.title title
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.meta ! A.name "robots" ! A.content "noindex"
  H.link ! A.rel "stylesheet" ! A.href "https://design.smart.coop/css/main.css"

myBody body = H.body ! A.class_ "u-maximize-height u-overflow-hidden" $ do
  H.div ! A.class_ "c-app-layout" $ body
  js

js = do
  H.script ! A.src "https://design.smart.coop/js/bundle-prototype.js" $ ""
  H.script ! A.src "https://design.smart.coop/js/bundle-client.js" $ ""



--------------------------------------------------------------------------------
-- The name can be referenced by `data-dialog` attribute to trigger the dialog.
-- It is also use to create an id and reference it by aria-labelledby.
dialogFullscreen :: Text -> Html -> Html
dialogFullscreen name content =
  H.div ! A.id (H.toValue name) ! A.class_ "c-dialog-context" $ do
    H.div
      ! A.class_ "c-dialog-backdrop"
      ! A.style "position:absolute;z-index:0;"
      $ ""
    H.div
      ! A.class_ "c-dialog c-dialog--fullscreen"
      ! A.role "dialog"
      ! customAttribute "aria-labelledby" (H.toValue $ name <> "-title")
      $ content

dialog name content = H.div ! A.class_ "c-dialog-context c-dialog-context" $ do
  H.div ! A.class_ "c-dialog-backdrop c-dialog-backdrop" $ ""
  H.div
    ! A.class_ "c-dialog c-dialog--medium"
    ! A.role "dialog"
    ! customAttribute "aria-labelledby" (H.toValue $ name ++ "-title")
    $ content

dialogVisible name content =
  H.div ! A.class_ "c-dialog-context c-dialog-context--visible" $ do
    H.div ! A.class_ "c-dialog-backdrop c-dialog-backdrop--visible" $ ""
    H.div
      ! A.class_ "c-dialog c-dialog--medium"
      ! A.role "dialog"
      ! customAttribute "aria-labelledby" (H.toValue $ name ++ "-title")
      $ content

dialogContent :: Text -> Html
dialogContent name = do
  H.div ! A.class_ "c-dialog__push" $ ""
  H.div
    ! A.class_ "c-dialog__header c-dialog__header--bordered"
    $ H.div
    ! A.class_ "c-toolbar c-toolbar--spaced"
    $ do
        H.div
          ! A.class_ "c-toolbar__left"
          $ H.div
          ! A.class_ "c-toolbar__item"
          $ H.h2
          ! A.class_ "c-dialog__title"
          ! A.id (H.toValue $ name <> "-title")
          $ "Dialog title"
        H.div
          ! A.class_ "c-toolbar__item"
          $ H.button
          ! A.class_ "c-button c-button--borderless c-button--icon"
          ! customAttribute "data-dialog-close aria-label " "Close dialog"
          $ H.div
          ! A.class_ "c-button__content"
          $ H.div
          ! A.class_ "o-svg-icon o-svg-icon-close  "
          $ H.toMarkup svgIconClose
  H.div
    ! A.class_ "c-dialog__body"
    $ H.div
    ! A.class_ "u-spacer-l"
    $ H.div
    ! A.class_ "o-form-group-layout o-form-group-layout--standard"
    $ do
        H.div ! A.class_ "o-form-group" $ do
          H.label ! A.class_ "o-form-group__label" ! A.for "input" $ "Input"
          H.div
            ! A.class_ "o-form-group__controls"
            $ H.input
            ! A.class_ "c-input"
            ! A.type_ "text"
            ! A.id "input"
        H.div ! A.class_ "o-form-group" $ do
          H.label ! A.class_ "o-form-group__label" ! A.for "select" $ "Select"
          H.div ! A.class_ "o-form-group__controls" $ do
            H.div
              ! A.class_ "c-select-holder"
              $ H.select
              ! A.class_ "c-select"
              ! A.id "select"
              $ mapM_ (H.option . H.toHtml)
                      (["Choose an item", "A", "B", "C"] :: [Text])
            H.p
              ! A.class_ "c-form-help-text"
              $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed commodo accumsan risus."
        H.div ! A.class_ "o-form-group" $ do
          H.label
            ! A.class_ "o-form-group__label"
            ! A.for "textarea"
            $ "Textarea"
          H.div ! A.class_ "o-form-group__controls" $ do
            H.textarea
              ! A.class_ "c-textarea"
              ! A.rows "5"
              ! A.id "textarea"
              $ ""
            H.p
              ! A.class_ "c-form-help-text"
              $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed commodo accumsan risus."
  H.div
    ! A.class_ "c-dialog__footer c-dialog__footer--bordered"
    $ H.div
    ! A.class_ "c-toolbar c-toolbar--spaced"
    $ do
        H.div
          ! A.class_ "c-toolbar__left"
          $ H.div
          ! A.class_ "c-toolbar__item"
          $ ""
        H.div
          ! A.class_ "c-toolbar__right"
          $ H.div
          ! A.class_ "c-toolbar__item"
          $ H.div
          ! A.class_ "c-button-toolbar"
          $ do
              H.button
                ! A.class_ "c-button c-button--secondary"
                ! customAttribute "data-dialog-close" "close"
                $ H.div
                ! A.class_ "c-button__content"
                $ H.div
                ! A.class_ "c-button__label"
                $ "Cancel"
              H.button
                ! A.class_ "c-button c-button--primary"
                ! customAttribute "data-dialog-close" "close"
                $ H.div
                ! A.class_ "c-button__content"
                $ H.div
                ! A.class_ "c-button__label"
                $ "Save"
  H.div ! A.class_ "c-dialog__push" $ ""


--------------------------------------------------------------------------------
alertTitleAndContent :: Text -> Html -> Html
alertTitleAndContent title content =
  H.div ! A.class_ "c-alert c-alert--default" $ do
    H.div
      ! A.class_ "o-svg-icon o-svg-icon-circle-information  "
      $ H.toMarkup
      $ svgIconCircleInformation
    H.div ! A.class_ "c-alert__body" $ H.div ! A.class_ "c-alert__text" $ do
      H.h4 ! A.class_ "c-alert__title" $ H.toHtml title
      H.div
        ! A.class_ "c-alert__message"
        $ H.div
        ! A.class_ "c-content"
        $ content


--------------------------------------------------------------------------------
banner = H.div ! A.class_ "c-global-banner c-global-banner--default" $ do
  H.div
    ! A.class_ "o-svg-icon o-svg-icon-circle-information o-svg-icon--default "
    $ H.toMarkup
    $ svgIconCircleInformation
  H.div ! A.class_ "c-global-banner__label" $ H.p
    "Nam eget hendrerit massa, a consequat turpis."
  H.button
    ! A.class_ "c-button c-button--borderless c-button--icon"
    ! A.type_ "button"
    ! customAttribute "data-banner-close" "data-banner-close"
    $ H.span
    ! A.class_ "c-button__content"
    $ do
        H.div ! A.class_ "o-svg-icon o-svg-icon-close  " $ H.toMarkup
          svgIconClose
        H.div ! A.class_ "u-sr-accessible" $ "Close"


--------------------------------------------------------------------------------
countries :: [Text]
countries =
  [ "Afghanistan"
  , "Aland Islands"
    -- TODO "Åland Islands"
  , "Albania"
  , "Algeria"
  , "American Samoa"
  , "Andorra"
  , "Angola"
  , "Anguilla"
  , "Antarctica"
  , "Antigua and Barbuda"
  , "Argentina"
  , "Armenia"
  , "Aruba"
  , "Australia"
  , "Austria"
  , "Azerbaijan"
  , "Bahamas"
  , "Bahrain"
  , "Bangladesh"
  , "Barbados"
  , "Belarus"
  , "Belgium"
  , "Belize"
  , "Benin"
  , "Bermuda"
  , "Bhutan"
  , "Bolivia (Plurinational State of)"
  , "Bonaire, Sint Eustatius and Saba[d]"
  , "Bosnia and Herzegovina"
  , "Botswana"
  , "Bouvet Island"
  , "Brazil"
  , "British Indian Ocean Territory"
  , "Brunei Darussalam"
  , "Bulgaria"
  , "Burkina Faso"
  , "Burundi"
  , "Cabo Verde"
  , "Cambodia"
  , "Cameroon"
  , "Canada"
  , "Cayman Islands"
  , "Central African Republic"
  , "Chad"
  , "Chile"
  , "China"
  , "Christmas Island"
  , "Cocos (Keeling) Islands"
  , "Colombia"
  , "Comoros"
  , "Congo"
  , "Congo, Democratic Republic of the"
  , "Cook Islands"
  , "Costa Rica"
  , "Cote d'Ivoire"
    -- TODO "Côte d'Ivoire"
  , "Croatia"
  , "Cuba"
  , "Curacao"
    -- TODO "Curaçao"
  , "Cyprus"
  , "Czechia"
  , "Denmark"
  , "Djibouti"
  , "Dominica"
  , "Dominican Republic"
  , "Ecuador"
  , "Egypt"
  , "El Salvador"
  , "Equatorial Guinea"
  , "Eritrea"
  , "Estonia"
  , "Eswatini"
  , "Ethiopia"
  , "Falkland Islands (Malvinas)"
  , "Faroe Islands"
  , "Fiji"
  , "Finland"
  , "France"
  , "French Guiana"
  , "French Polynesia"
  , "French Southern Territories"
  , "Gabon"
  , "Gambia"
  , "Georgia"
  , "Germany"
  , "Ghana"
  , "Gibraltar"
  , "Greece"
  , "Greenland"
  , "Grenada"
  , "Guadeloupe"
  , "Guam"
  , "Guatemala"
  , "Guernsey"
  , "Guinea"
  , "Guinea-Bissau"
  , "Guyana"
  , "Haiti"
  , "Heard Island and McDonald Islands"
  , "Holy See"
  , "Honduras"
  , "Hong Kong"
  , "Hungary"
  , "Iceland"
  , "India"
  , "Indonesia"
  , "Iran (Islamic Republic of)"
  , "Iraq"
  , "Ireland"
  , "Isle of Man"
  , "Israel"
  , "Italy"
  , "Jamaica"
  , "Japan"
  , "Jersey"
  , "Jordan"
  , "Kazakhstan"
  , "Kenya"
  , "Kiribati"
  , "Korea (Democratic People's Republic of)"
  , "Korea, Republic of"
  , "Kuwait"
  , "Kyrgyzstan"
  , "Lao People's Democratic Republic"
  , "Latvia"
  , "Lebanon"
  , "Lesotho"
  , "Liberia"
  , "Libya"
  , "Liechtenstein"
  , "Lithuania"
  , "Luxembourg"
  , "Macao"
  , "Madagascar"
  , "Malawi"
  , "Malaysia"
  , "Maldives"
  , "Mali"
  , "Malta"
  , "Marshall Islands"
  , "Martinique"
  , "Mauritania"
  , "Mauritius"
  , "Mayotte"
  , "Mexico"
  , "Micronesia (Federated States of)"
  , "Moldova, Republic of"
  , "Monaco"
  , "Mongolia"
  , "Montenegro"
  , "Montserrat"
  , "Morocco"
  , "Mozambique"
  , "Myanmar"
  , "Namibia"
  , "Nauru"
  , "Nepal"
  , "Netherlands"
  , "New Caledonia"
  , "New Zealand"
  , "Nicaragua"
  , "Niger"
  , "Nigeria"
  , "Niue"
  , "Norfolk Island"
  , "North Macedonia"
  , "Northern Mariana Islands"
  , "Norway"
  , "Oman"
  , "Pakistan"
  , "Palau"
  , "Palestine, State of"
  , "Panama"
  , "Papua New Guinea"
  , "Paraguay"
  , "Peru"
  , "Philippines"
  , "Pitcairn"
  , "Poland"
  , "Portugal"
  , "Puerto Rico"
  , "Qatar"
  , "Reunion"
    -- TODO "Réunion"
  , "Romania"
  , "Russian Federation"
  , "Rwanda"
  , "Saint Barthelemy"
    -- TODO "Saint Barthélemy"
  , "Saint Helena, Ascension and Tristan da Cunha[e]"
  , "Saint Kitts and Nevis"
  , "Saint Lucia"
  , "Saint Martin (French part)"
  , "Saint Pierre and Miquelon"
  , "Saint Vincent and the Grenadines"
  , "Samoa"
  , "San Marino"
  , "Sao Tome and Principe"
  , "Saudi Arabia"
  , "Senegal"
  , "Serbia"
  , "Seychelles"
  , "Sierra Leone"
  , "Singapore"
  , "Sint Maarten (Dutch part)"
  , "Slovakia"
  , "Slovenia"
  , "Solomon Islands"
  , "Somalia"
  , "South Africa"
  , "South Georgia and the South Sandwich Islands"
  , "South Sudan"
  , "Spain"
  , "Sri Lanka"
  , "Sudan"
  , "Suriname"
  , "Svalbard and Jan Mayen[f]"
  , "Sweden"
  , "Switzerland"
  , "Syrian Arab Republic"
  , "Taiwan, Province of China"
  , "Tajikistan"
  , "Tanzania, United Republic of"
  , "Thailand"
  , "Timor-Leste"
  , "Togo"
  , "Tokelau"
  , "Tonga"
  , "Trinidad and Tobago"
  , "Tunisia"
  , "Turkey"
  , "Turkmenistan"
  , "Turks and Caicos Islands"
  , "Tuvalu"
  , "Uganda"
  , "Ukraine"
  , "United Arab Emirates"
  , "United Kingdom of Great Britain and Northern Ireland"
  , "United States of America"
  , "United States Minor Outlying Islands[h]"
  , "Uruguay"
  , "Uzbekistan"
  , "Vanuatu"
  , "Venezuela (Bolivarian Republic of)"
  , "Viet Nam"
  , "Virgin Islands (British)"
  , "Virgin Islands (U.S.)"
  , "Wallis and Futuna"
  , "Western Sahara"
  , "Yemen"
  , "Zambia"
  , "Zimbabwe"
  ]


--------------------------------------------------------------------------------
buttonBackSecondary =
  H.button
    ! A.class_ "c-button c-button--secondary"
    ! A.type_ "button"
    $ H.span
    ! A.class_ "c-button__content"
    $ do
        H.div
          ! A.class_ "o-svg-icon o-svg-icon-arrow-left  "
          $ H.toMarkup
          $ svgIconArrowLeft
        H.span ! A.class_ "c-button__label" $ "Back"

buttonNext =
  H.button
    ! A.class_ "c-button c-button--primary"
    ! A.type_ "button"
    $ H.span
    ! A.class_ "c-button__content"
    $ do
        H.span ! A.class_ "c-button__label" $ "Next"
        H.div
          ! A.class_ "o-svg-icon o-svg-icon-arrow-right  "
          $ H.toMarkup
          $ svgIconArrowRight

buttonAdd :: Text -> Html
buttonAdd label =
  H.button
    ! A.class_ "c-button c-button--primary"
    ! A.type_ "button"
    $ H.span
    ! A.class_ "c-button__content"
    $ do
        H.div ! A.class_ "o-svg-icon o-svg-icon-add  " $ H.toMarkup $ svgIconAdd
        H.span ! A.class_ "c-button__label" $ H.toHtml label

buttonAddSecondary :: Text -> Html
buttonAddSecondary label =
  H.button
    ! A.class_ "c-button c-button--secondary"
    ! A.type_ "button"
    $ H.span
    ! A.class_ "c-button__content"
    $ do
        H.div ! A.class_ "o-svg-icon o-svg-icon-add" $ H.toMarkup $ svgIconAdd
        H.span ! A.class_ "c-button__label" $ H.toHtml label

buttonSave :: Text -> Html
buttonSave label =
  H.button ! A.class_ "c-button c-button--primary" ! A.type_ "button" $ do
    H.span ! A.class_ "c-button__content" $ do
      H.div ! A.class_ "o-svg-icon o-svg-icon-save" $ do
        H.toMarkup $ svgIconSave
      H.span ! A.class_ "c-button__label" $ H.toHtml label

buttonClodeDangerSecondary :: Text -> Html
buttonClodeDangerSecondary label =
  H.button
    ! A.class_ "c-button c-button--danger-secondary"
    ! A.type_ "button"
    $ do
        H.span ! A.class_ "c-button__content" $ do
          H.div ! A.class_ "o-svg-icon o-svg-icon-close" $ do
            H.toMarkup svgIconClose
          H.span ! A.class_ "c-button__label" $ H.toHtml label


--------------------------------------------------------------------------------
idSelectFunction :: Text
idSelectFunction = "select-function"


--------------------------------------------------------------------------------
toolsNewContract :: Html
toolsNewContract = document "Smart design system - New contract" $ do
  mainContent (titlebar "New contract") $ do
    form
    dialogFullscreen idSelectFunction (dialogContent idSelectFunction)

form = vertically $ mapM_ (uncurry panel) [("Contract type", subform1')]

subform1' = groupHorizontal $ do
  inputDialog "position" "Your position"
  inputTextarea "description" "Description of the contract" 5 ""
  inputSelect "work-country" "Work country" countries
  inputRadios
    "has-risks"
    "Risks"
    [ ("This position involves risks."           , False)
    , ("This position doesn't involve any risks.", False)
    ]

-- Same as inputText, but defers the choice to a dialog.
inputDialog :: Text -> Text -> Html
inputDialog name label = H.div ! A.class_ "o-form-group" $ do
  H.label ! A.class_ "o-form-group__label" ! A.for (H.toValue name) $ H.toHtml
    label
  H.div ! A.class_ "c-input-group" $ do
    H.input
      ! A.class_ "c-input"
      ! A.type_ "text"
      ! A.id (H.toValue name)
      ! A.value "Webmaster"
      ! A.readonly "readonly"
    H.div
      ! A.class_ "c-input-group__append"
      ! customAttribute "data-dialog" (H.toValue idSelectFunction)
      $ H.div
      ! A.class_ "o-svg-icon o-svg-icon-edit"
      $ H.toMarkup svgIconEdit


--------------------------------------------------------------------------------
webEmpty :: Html
webEmpty = webDocument "Smart design system" $ return ()


--------------------------------------------------------------------------------
-- https://design.smart.coop/blog/2021/10/08/smart-announces-an-open-design-system.html
webPage :: Html
webPage = webDocument "Smart design system" $ article
  "Smart announces an open design system"
  (Just "October 8, 2021")
  post
  avatars

post = do
  H.p $ do
    "We're happy to announce we, at Smart Belgium, have started to work on a design system for the web applications of the Smart group. For the first time, but surely not the last, we've decided to make this project open source and available on "
    H.a ! A.href "https://github.com/smartcoop/design" $ "GitHub"
    ". We believe this matches the collaborative values of Smart and will enable various parties of the Smart ecosystem to benefit from it."
  H.p $ do
    "The home of the design system lives at the homepage of this very website. In addition to the blog that you're reading right now, you will be able to learn what's new, find the necessary documentation to get started with "
    H.a
      ! A.href "https://design.smart.coop/design/how-it-works.html"
      $ "designing"
    ", using and "
    H.a
      ! A.href "https://design.smart.coop/development/getting-started.html"
      $ "developing"
    " the design system yourself, as well as find the right links to be able to contribute."
  H.p "With this design system we are pursuing three goals:"
  H.ul $ do
    H.li
      "The first goal is to create the visual basis of our future software developments. This may mean the different colors that can be used, the shape and size of a button, or the location of a menu or a form on the screen. This visual work follows and stands on top of the graphic charter introduced in early 2019."
    H.li
      "The second goal is to create and assemble those design elements in a tool called Figma. Figma is specialized to design software interfaces. In addition of design work by designers, it can also be used by software development teams to prototype what new screens could look like and how they are organized."
    H.li
      "The third goal is to implement and present the design using web technologies: HTML and CSS. This implementation is meant to be a source of truth for other developers, so they can replicate the design in whatever programming language and technical stack they want."
  H.p $ do
    "We're very excited by what we've done so far and will write additional blog posts soon. In the mean time, you can already join the conversation using "
    H.a
      ! A.href "https://github.com/smartcoop/design/issues/111"
      $ "the GitHub issue for commenting for this post"
    ". Feel free to raise your own issues to give feedback and comment on the design system, or start your first contribution."

avatars =
  H.div
    ! A.class_ "o-container-vertical o-container-vertical--padding-mini"
    $ H.ul
    ! A.class_ "c-avatar-and-text-list"
    $ do
        H.li ! A.class_ "c-avatar-and-text" $ do
          H.a
            ! A.class_ "c-avatar c-avatar--img c-avatar--regular"
            ! A.href "https://github.com/thusc/"
            $ H.img
            ! A.src "https://avatars.githubusercontent.com/u/45588452?v=4"
            ! A.alt "avatar-image"
          H.div ! A.class_ "c-avatar-and-text__text" $ H.p "Thu"
        H.li ! A.class_ "c-avatar-and-text" $ do
          H.a
            ! A.class_ "c-avatar c-avatar--img c-avatar--regular"
            ! A.href "https://github.com/Wolfr/"
            $ H.img
            ! A.src "https://avatars.githubusercontent.com/u/12690?v=4"
            ! A.alt "avatar-image"
          H.div ! A.class_ "c-avatar-and-text__text" $ H.p "Wolfr"


--------------------------------------------------------------------------------
webDocument title body = do
  H.docType
  H.html ! A.class_ "u-maximize-height" ! A.dir "ltr" ! A.lang "en" $ do
    myHead title
    webBody body

article title mdate content authors =
  H.article ! A.class_ "c-blog-article" $ do
    H.div ! A.class_ "c-blog-article__header" $ do
      H.h1 ! A.class_ "c-d-h1" $ title
      maybe (return ()) H.p mdate
    H.div ! A.class_ "c-blog-article__content" $ do
      H.div ! A.class_ "c-display" $ content
      authors


--------------------------------------------------------------------------------
webBody body = H.body ! A.class_ "u-maximize-height" $ do
  myHeader
  H.main
    ! A.class_ "o-container"
    $ H.div
    ! A.class_ "o-container o-container--medium"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ body
  myFooter
  js

myHeader =
  H.header
    ! A.id "header"
    $ H.div
    ! A.class_ "o-container"
    $ H.div
    ! A.class_ "c-navbar c-navbar--bordered-bottom c-navbar--main"
    $ H.div
    ! A.class_ "c-toolbar"
    $ do
        H.div
          ! A.class_ "c-toolbar__left"
          $ H.div
          ! A.class_ "c-toolbar__item"
          $ H.div
          ! A.class_ "c-brand c-brand--small"
          $ H.a
          ! A.href "/"
          $ H.img
          ! A.src "https://design.smart.coop/images/logo.svg"
          ! A.alt "Smart"
        H.div
          ! A.class_ "c-toolbar__right"
          $ H.div
          ! A.class_ "c-toolbar__item"
          $ H.nav
          ! A.class_ "c-design-system-nav"
          $ do
              H.button
                ! A.class_
                    "c-button c-button--borderless c-button--icon c-design-system-nav-open"
                ! A.type_ "button"
                ! A.id "c-design-system-nav-open"
                $ H.span
                ! A.class_ "c-button__content"
                $ do
                    H.div
                      ! A.class_ "o-svg-icon o-svg-icon-menu  "
                      $ H.toMarkup svgIconMenu
                    H.div ! A.class_ "u-sr-accessible" $ "Open menu"
              H.button
                ! A.class_
                    "c-button c-button--borderless c-button--icon c-design-system-nav-close"
                ! A.type_ "button"
                ! A.id "c-design-system-nav-close"
                $ H.span
                ! A.class_ "c-button__content"
                $ do
                    H.div
                      ! A.class_ "o-svg-icon o-svg-icon-close  "
                      $ H.toMarkup svgIconClose
                    H.div ! A.class_ "u-sr-accessible" $ "Close menu"
              H.div ! A.class_ "c-design-system-nav__mobile" $ H.ul $ do
                H.li $ do
                  H.a ! A.href "/components/" $ "Components"
                H.li $ H.a ! A.href "/pages/" $ "Pages"
              H.div
                ! A.class_ "c-design-system-nav__desktop"
                $ H.ul
                ! A.class_ "c-pill-navigation"
                $ do
                    H.li
                      ! A.class_ "c-pill-navigation__item"
                      $ H.a
                      ! A.href "/components/"
                      $ "Components"
                    H.li
                      ! A.class_ "c-pill-navigation__item"
                      $ H.a
                      ! A.href "/pages/"
                      $ "Pages"

myFooter =
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
          ! A.href "https://github.com/smartcoop/design"
          $ H.div
          ! A.class_ "o-flex"
          $ do
              H.div
                ! A.class_ "u-spacer-right-s"
                $ H.div
                ! A.class_ "o-svg-icon o-svg-icon-github"
                $ H.toMarkup svgIconGitHub
              "Share feedback on GitHub"
