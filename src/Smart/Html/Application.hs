{-# LANGUAGE OverloadedStrings #-}

module Smart.Html.Application where

import Control.Monad (forM_)
import Text.Blaze (customAttribute)
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (preEscapedToHtml, toHtml, (!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Svg as S (toSvg)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as SA

import Smart.Html.Alerts
import Smart.Html.Banners
import Smart.Html.Data
import Smart.Html.Buttons
import Smart.Html.Icons


--------------------------------------------------------------------------------
empty :: Html
empty = document "Smart design system" $ do
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
  mainContent toolbar (panels >> dialogVisible "dialog" (dialogContent "dialog"))


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
  H.html
    ! A.class_ "u-maximize-height"
    ! A.dir "ltr"
    ! A.lang "en" $ do
    myHead title
    H.body ! A.class_ "u-maximize-height" $ do
      H.div ! A.class_ "u-spacer-l" $
        H.div ! A.class_ "o-container-vertical" $
          H.div ! A.class_ "o-container o-container--medium" $ do
            H.div ! A.class_ "u-spacer-bottom" $
              H.div ! A.class_ "c-brand c-brand--small" $
                H.a ! A.href "/" $
                  H.img ! A.src "https://design.smart.coop/images/logo.svg" ! A.alt "Smart"
            H.div ! A.class_ "c-hr" $ ""
            H.h1 ! A.class_ "c-h2" $ "Register your account"
            H.div ! A.class_ "c-content" $
              H.p $ do
                "For optimal use of the Smart services, it is highly recommended to attend a Smart session: view our "
                H.a ! A.href "#" $ "user manual"
                "."
            H.div ! A.class_ "u-spacer-bottom" $ do
              alertTitleAndContent "Important" $
                H.ul $ do
                  H.li "You will receive your personal codes to access your ???Smart account??? (and to fill in your contracts) by email within 48 working hours of sending your registration documents. Do not hesitate to contact the nearest Smart office in case of emergency."
                  H.li "Remember to always declare your working days in advance. It is not allowed to backdate your services."

            H.div ! A.class_ "o-form-group-layout o-form-group-layout--standard" $ do
              H.div ! A.class_ "o-grid" $ do
                inputText6 "registerFormName" "Name"
                inputText6 "registerFormFirstName" "First name"
                inputSelect6 "registerFormGender" "Gender"
                  [ "Male", "Female", "X" ]
                inputSelect6 "registerFormLangue" "Language"
                  [ "Fran??ais", "Nederlands", "Deutsch", "Espanol" ]

              H.div ! A.class_ "c-hr" $ ""
              H.h3 ! A.class_ "c-h3" $ "Official address"
              H.div ! A.class_ "o-grid" $ do
                inputText8 "registerFormAddressOfficalStreet" "Street"
                inputText2 "registerFormAddressOfficalStreetNo" "Number"
                inputText2 "registerFormAddressOfficalStreetBuildingAddition" "Addition"
                inputText6 "registerFormAddressOfficalZipCode" "Postal code"
                inputSelect6 "registerFormAddressOfficalCountry" "Country"
                  countries

              H.div ! A.class_ "c-hr" $ ""
              H.h3 ! A.class_ "c-h3" $ "Mailing address"
              H.div ! A.class_ "o-grid" $ do
                inputText8 "registerFormAddressPostalStreet" "Street"
                inputText2 "registerFormAddressPostalStreetNo" "Number"
                inputText2 "registerFormAddressPostalStreetBuildingAddition" "Addition"
                inputText6 "registerFormAddressPostalZipCode" "Postal code"
                inputSelect6 "registerFormAddressPostalCountry" "Country"
                  countries

              H.div ! A.class_ "c-hr" $ ""
              H.div ! A.class_ "o-grid" $ do
                inputText6 "registerFormCellPhoneNumber" "Mobile number"
                inputText6 "registerFormHomePhoneNumber" "Home phone number"
                inputEmail "registerFormfieldEmail" "Email"
                inputDate "registerFormfieldBirthdate" "Birthdate"
                inputSelect6 "registerFormNativeCountry" "Native country"
                  countries
                inputSelect6 "registerFormfieldMaritalStatus" "Marital status"
                  [ "Select", "Single", "Married"
                  , "Widowed", "Separated", "Divorced" ]
                inputText6 "registerFormfieldCityOfBirth" "City of birth"
                inputText6 "registerFormfieldNationalRegisterNo" "National register number"
                inputText6 "registerFormfieldIdentityCardNumber" "Identity card number"
                inputText6 "registerFormfieldIbanNo" "IBAN number"
                inputText6 "registerFormfieldProfessionalWithholdingTax" "Professional withholding tax"
                inputText6 "registerFormfieldMainFunction" "Main function"
                inputText6 "registerFormfieldOtherFunction" "Other function"

              H.div ! A.class_ "o-form-group" $ do
                H.div ! A.class_ "c-checkbox" $ do
                  H.label $ do
                    H.input ! A.type_ "checkbox" ! A.checked "checked"
                    H.div $ do
                      "I authorise Smart to transmit my data to third parties for professional purposes only."
                      H.a ! A.class_ "o-flex o-flex--vertical-center" ! A.href "#" $ do
                        H.div ! A.class_ "u-spacer-right-s" $ "Read more here"
                        H.div ! A.class_ "o-svg-icon o-svg-icon-external-link  " $
                          svgIconExternalLink
              H.div ! A.class_ "o-form-group" $
                H.input ! A.class_ "c-button c-button--primary c-button--block" ! A.type_ "submit" ! A.value "Continue"

    js


--------------------------------------------------------------------------------
inputText :: String -> String -> Html
inputText name label =
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" ! A.for (H.toValue name) $
      H.toHtml label
    H.div ! A.class_ "o-form-group__controls" $
      H.input ! A.class_ "c-input" ! A.type_ "text" ! A.id (H.toValue name)

inputText2 :: String -> String -> Html
inputText2 name label =
  H.div ! A.class_ "o-grid-col-2" $
    inputText name label

inputText6 :: String -> String -> Html
inputText6 name label =
  H.div ! A.class_ "o-grid-col-6" $
    inputText name label

inputText8 :: String -> String -> Html
inputText8 name label =
  H.div ! A.class_ "o-grid-col-8" $
    inputText name label

inputEmail :: String -> String -> Html
inputEmail name label =
  H.div ! A.class_ "o-grid-col-6" $
    H.div ! A.class_ "o-form-group" $ do
      H.label ! A.class_ "o-form-group__label" ! A.for (H.toValue name) $
        H.toHtml label
      H.div ! A.class_ "o-form-group__controls" $
        H.input ! A.class_ "c-input" ! A.type_ "email" ! A.id (H.toValue name)

inputDate :: String -> String -> Html
inputDate name label =
  H.div ! A.class_ "o-grid-col-6" $
    H.div ! A.class_ "o-form-group" $ do
      H.label ! A.class_ "o-form-group__label" ! A.for (H.toValue name) $
        H.toHtml label
      H.div ! A.class_ "o-form-group__controls" $
        H.input ! A.class_ "c-input" ! A.type_ "date" ! A.id (H.toValue name)

inputSelect_ :: String -> String -> [String] -> Maybe String -> Html
inputSelect_ name label values mhelp =
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" ! A.for (H.toValue name) $
      H.toHtml label
    H.div ! A.class_ "o-form-group__controls" $ do
      H.div ! A.class_ "c-select-holder" $
        H.select ! A.class_ "c-select" ! A.id (H.toValue name) $
          mapM_ (H.option . H.toHtml) values
      maybe (return ()) (\s -> H.p ! A.class_ "c-form-help-text" $ H.toHtml s) mhelp

inputSelect :: String -> String -> [String] -> Html
inputSelect name label values = inputSelect_ name label values Nothing

inputSelect' :: String -> String -> [String] -> String -> Html
inputSelect' name label values help = inputSelect_ name label values (Just help)

inputSelect6 :: String -> String -> [String] -> Html
inputSelect6 name label values =
  H.div ! A.class_ "o-grid-col-6" $ do
    inputSelect name label values

inputTextarea :: String -> String -> Int -> String -> Html
inputTextarea name label rows help =
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" ! A.for (H.toValue name) $
      H.toHtml label
    H.div ! A.class_ "o-form-group__controls" $ do
      H.textarea ! A.class_ "c-textarea"
        ! A.rows (H.toValue $ show rows)
        ! A.id (H.toValue name) $ ""
      H.p ! A.class_ "c-form-help-text" $
        H.toHtml help

inputRadios :: String -> String -> [(String, Bool)] -> Html
inputRadios name label labels =
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" $
      H.toHtml label
    H.div ! A.class_ "o-form-group__controls" $
      H.div ! A.class_ "c-radio-group" $ do
        mapM_ radio labels
  where
  radio (label, checked) =
    H.div ! A.class_ "c-radio" $
      H.label $ do
        if checked
          then
            H.input ! A.type_ "radio" ! A.name (H.toValue name) ! A.checked "checked"
          else
            H.input ! A.type_ "radio" ! A.name (H.toValue name)
        H.toHtml label


--------------------------------------------------------------------------------
mainContent top content =
  H.main ! A.class_ "u-scroll-wrapper u-maximize-width" $ do
    top
    H.div ! A.class_ "u-scroll-wrapper-body" $
      content

mainContentSideMenu menu top content =
  H.main ! A.class_ "u-scroll-wrapper u-maximize-width" $ do
    H.div ! A.class_ "c-app-layout-inner" $ do
      H.div ! A.class_ "c-app-layout-inner__sidebar u-bg-gray-50" $
        menu
      H.div ! A.class_ "c-app-layout-inner__main" $
        H.div ! A.class_ "u-scroll-wrapper" $ do
          top
          H.div ! A.class_ "u-scroll-wrapper-body" $
            content


--------------------------------------------------------------------------------
toolbar =
  H.div ! A.class_ "c-navbar c-navbar--bordered-bottom" $
    H.div ! A.class_ "c-toolbar" $ do
      H.div ! A.class_ "c-toolbar__left" $ do
        H.div ! A.class_ "c-toolbar__item" $ do
          H.a ! A.class_ "c-button c-button--icon c-button--borderless" ! A.href "#" $ do
            H.div ! A.class_ "c-button__content" $ do
              H.div ! A.class_ "o-svg-icon o-svg-icon-arrow-left" $ do
                svgIconArrowLeft
              H.div ! A.class_ "u-sr-accessible" $ "Back"
        H.div ! A.class_ "c-toolbar__item" $ do
          H.h2 ! A.class_ "c-toolbar__title" $ "Toolbar title"
      H.div ! A.class_ "c-toolbar__right" $ do
        H.div ! A.class_ "c-toolbar__item" $ do
          H.div ! A.class_ "c-button-toolbar" $ do
            buttonClodeDangerSecondary "Cancel"
            buttonSave "Save"

wizard =
  H.div ! A.class_ "c-navbar c-navbar--bordered-bottom" $
    H.div ! A.class_ "c-toolbar" $ do
      H.div ! A.class_ "c-toolbar__left" $
        H.div ! A.class_ "c-toolbar__item" $
          H.nav ! A.class_ "c-wizard" $
            H.ul $ do
              H.li $
                H.a ! A.class_ "c-wizard__item c-wizard--complete" ! A.href "#" $ do
                  H.div ! A.class_ "c-wizard__indicator" $ ""
                  H.div ! A.class_ "c-wizard__label" $ "General info"
              H.li $
                H.a ! A.class_ "c-wizard__item c-wizard--active" ! A.href "#" $ do
                  H.div ! A.class_ "c-wizard__indicator" $ "2"
                  H.div ! A.class_ "c-wizard__label" $ "Location and dates"
              H.li $
                H.a ! A.class_ "c-wizard__item" ! A.href "#" $ do
                  H.div ! A.class_ "c-wizard__indicator" $ "3"
                  H.div ! A.class_ "c-wizard__label" $ "Function and risks"
              H.li $
                H.a ! A.class_ "c-wizard__item" ! A.href "#" $ do
                  H.div ! A.class_ "c-wizard__indicator" $ "4"
                  H.div ! A.class_ "c-wizard__label" $ "Contract type"
              H.li $
                H.a ! A.class_ "c-wizard__item" ! A.href "#" $ do
                  H.div ! A.class_ "c-wizard__indicator" $ "5"
                  H.div ! A.class_ "c-wizard__label" $ "Confirm"
      H.div ! A.class_ "c-toolbar__right" $
        H.div ! A.class_ "c-toolbar__item" $
          H.div ! A.class_ "c-button-toolbar" $ do
            buttonBackSecondary
            buttonNext

titlebar :: String -> Html
titlebar title =
  H.div ! A.class_ "c-navbar c-navbar--bordered-bottom" $
    H.div ! A.class_ "c-toolbar" $ do
      H.div ! A.class_ "c-toolbar__left" $
        H.div ! A.class_ "c-toolbar__item" $
          H.h2 ! A.class_ "c-toolbar__title" $
            H.toHtml title
      H.div ! A.class_ "c-toolbar__right" $
        H.div ! A.class_ "c-toolbar__item" $
          H.div ! A.class_ "c-button-toolbar" $
            buttonAdd "Add item"


--------------------------------------------------------------------------------
menu =
  H.ul ! A.class_ "c-side-menu" $ do
    H.li ! A.class_ "c-side-menu__item c-side-menu__item--active" $
      H.a ! A.class_ "c-side-menu__link" ! A.href "#" $ do
        H.div ! A.class_ "o-svg-icon o-svg-icon-document  " $
          svgIconDocument
        H.div ! A.class_ "c-sidebar-item__label" $ "Quotes & invoices"
    H.li ! A.class_ "c-side-menu__item" $
      H.a ! A.class_ "c-side-menu__link" ! A.href "#" $ do
        H.div ! A.class_ "o-svg-icon o-svg-icon-bills  " $
          svgIconBills
        H.div ! A.class_ "c-sidebar-item__label" $ "Funding"
    H.li ! A.class_ "c-side-menu__item" $
      H.a ! A.class_ "c-side-menu__link" ! A.href "#" $ do
        H.div ! A.class_ "o-svg-icon o-svg-icon-tag  " $
          svgIconTag
        H.div ! A.class_ "c-sidebar-item__label" $ "Expenses"


--------------------------------------------------------------------------------
panels = panels_ Nothing

panels' = panels_ . Just

panels_ mtitle =
  vertically $ do
    maybe (return ()) (\t ->
      H.div ! A.class_ "c-content" $
        H.h1 t) mtitle
    mapM_ (panel "Form grouping") [subform1, subform2, subform3]

vertically content =
  H.div ! A.class_ "o-container o-container--large" $
    H.div ! A.class_ "o-container-vertical" $
      content

group content =
  H.div ! A.class_ "o-form-group-layout o-form-group-layout--standard" $
    content

groupHorizontal content =
  H.div ! A.class_ "o-form-group-layout o-form-group-layout--horizontal" $
    content

panel :: String -> Html -> Html
panel title content =
  H.div ! A.class_ "c-panel u-spacer-bottom-l" $ do
    H.div ! A.class_ "c-panel__header" $
      H.h2 ! A.class_ "c-panel__title" $
        H.toHtml title
    H.div ! A.class_ "c-panel__body" $ do
      content

subform1 =
  group $ do
    H.div ! A.class_ "o-form-group" $ do
      H.label ! A.class_ "o-form-group__label" $ "Add client"
      H.div ! A.class_ "c-empty-state c-empty-state--bg-alt" $ do
        H.p ! A.class_ "u-text-muted c-body-1" $ "Please add a client for this quote."
        H.div ! A.class_ "c-button-toolbar" $ do
          buttonAddSecondary "Add new client"
          buttonAddSecondary "Add existing client"
    inputRadios "radio1" "Radio"
      [ ("Lorem ipsum dolor sit amet.", True)
      , ("Lorem ipsum dolor sit amet.", False)
      ]

subform2 =
  group $ do
    inputText "input" "Input"
    inputSelect' "select" "Select"
      [ "Choose an item", "A", "B", "C" ]
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed commodo accumsan risus."
    inputTextarea "textarea" "Textarea" 5
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed commodo accumsan risus."

subform3 =
  group $ do
    H.div ! A.class_ "o-form-group" $ do
      H.label ! A.class_ "o-form-group__label" ! A.for "input" $ "Nr BCE"
      H.div ! A.class_ "o-form-group__controls" $
        H.input ! A.class_ "c-input" ! A.type_ "text" ! A.placeholder "__/__/__" ! A.id "input"
    H.div ! A.class_ "o-form-group" $ do
      H.label ! A.class_ "o-form-group__label" ! A.for "saleAmount" $ "Sale amount"
      H.div ! A.class_ "c-input-group" $ do
        H.input ! A.class_ "c-input" ! A.type_ "number" ! A.id "saleAmount"
        H.div ! A.class_ "c-input-group__append" $ "???"
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
      H.div ! A.class_ "o-form-group__controls" $
        H.input ! A.class_ "c-input" ! A.type_ "date" ! A.id "date"


--------------------------------------------------------------------------------
table =
  H.div ! A.class_ "u-padding-horizontal-s" $ do
    H.table ! A.class_ "c-table c-table--styled js-data-table" $ do
      H.thead $
        H.tr $ do
          H.th "Data"
          H.th "Data"
          H.th "Data"
          H.th ""
      H.tbody $
        forM_ [1..10] $ \_ ->
          H.tr $ do
            H.td "Data"
            H.td "Data"
            H.td "Data"
            H.td $
              rowAction
    pagination

rowAction =
  H.div ! A.class_ "c-button-toolbar" $ do
    H.button
      ! A.class_ "c-button c-button--borderless c-button--icon"
      ! A.type_ "button"
      ! customAttribute "data-menu-placement" "bottom-end"
      ! customAttribute "data-menu" "dropdownMenu-0" $
      H.span ! A.class_ "c-button__content" $ do
        H.div ! A.class_ "o-svg-icon o-svg-icon-options-horizontal" $
          svgIconOptionsHorizontal
        H.div ! A.class_ "u-sr-accessible" $ "More options"
    H.ul ! A.class_ "c-menu" ! A.id "dropdownMenu-0" $ do
      H.li ! A.class_ "c-menu__item" $
        H.a ! A.class_ "c-menu__label" ! A.href "#" $ do
          H.div ! A.class_ "o-svg-icon o-svg-icon-edit" $
            svgIconEdit
          H.span $ "Edit"
      H.li ! A.class_ "c-menu__item" $
        H.a ! A.class_ "c-menu__label" ! A.href "#" $ do
          H.div ! A.class_ "o-svg-icon o-svg-icon-delete" $
            svgIconDelete
          H.span "Delete"
    H.button ! A.class_ "c-button c-button--borderless c-button--icon" ! A.type_ "button" $
      H.span ! A.class_ "c-button__content" $ do
        H.div ! A.class_ "o-svg-icon o-svg-icon-chevron-right" $
          svgIconChevronRight
        H.div ! A.class_ "u-sr-accessible" $ "Go to detail"

pagination =
  H.div ! A.class_ "u-padding-horizontal" $
    H.div ! A.class_ "u-spacer-top" $
      H.div ! A.class_ "c-toolbar c-toolbar--auto" $ do
        H.div ! A.class_ "c-toolbar__left" $
          H.div ! A.class_ "c-toolbar__item" $
            H.div ! A.class_ "c-pagination-simple" $ do
              H.div ! A.class_ "c-pagination-simple__item" $ "1 of 4"
              H.div ! A.class_ "c-pagination-simple__item" $
                H.div ! A.class_ "c-button-toolbar" $ do
                  H.button ! A.class_ "c-button c-button--icon c-button--borderless" ! A.disabled "disabled" $
                    H.div ! A.class_ "c-button__content" $ do
                      H.div ! A.class_ "o-svg-icon o-svg-icon-chevron-left" $
                        svgIconChevronLeft
                      H.div ! A.class_ "u-sr-accessible" $ "Previous"
                  H.button ! A.class_ "c-button c-button--icon c-button--borderless" $
                    H.div ! A.class_ "c-button__content" $ do
                      H.div ! A.class_ "o-svg-icon o-svg-icon-chevron-right" $
                        svgIconChevronRight
                      H.div ! A.class_ "u-sr-accessible" $ "Next"
        H.div ! A.class_ "c-toolbar__right" $
          H.div ! A.class_ "c-toolbar__item" $
            H.div ! A.class_ "o-form-group-layout o-form-group-layout--horizontal" $
              H.div ! A.class_ "o-form-group" $ do
                H.label ! A.class_ "u-spacer-right-s" ! A.for "itemsPerPageId" $ "Items per page"
                H.div ! A.class_ "o-form-group__controls" $
                  H.div ! A.class_ "c-select-holder" $
                    H.select ! A.class_ "c-select" ! A.id "itemsPerPageId" $ do
                      H.option "10"
                      H.option "20"
                      H.option "50"


--------------------------------------------------------------------------------
document title body = do
  H.docType
  H.html
    ! A.class_ "u-maximize-height"
    ! A.dir "ltr"
    ! A.lang "en" $ do
    myHead title
    myBody body


--------------------------------------------------------------------------------
myHead title =
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.title title
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.meta ! A.name "robots" ! A.content "noindex"
    H.link
      ! A.rel "stylesheet"
      ! A.href "https://design.smart.coop/css/main.css"

myBody body =
  H.body ! A.class_ "u-maximize-height u-overflow-hidden" $ do
    H.div ! A.class_ "c-app-layout" $
      body
    js

js = do
  H.script ! A.src "https://design.smart.coop/js/bundle-prototype.js" $ ""
  H.script ! A.src "https://design.smart.coop/js/bundle-client.js" $ ""



--------------------------------------------------------------------------------
-- The name can be referenced by `data-dialog` attribute to trigger the dialog.
-- It is also use to create an id and reference it by aria-labelledby.
dialogFullscreen name content =
  H.div ! A.id (H.toValue name) ! A.class_ "c-dialog-context" $ do
    H.div ! A.class_ "c-dialog-backdrop"
      ! A.style "position:absolute;z-index:0;" $ ""
    H.div ! A.class_ "c-dialog c-dialog--fullscreen" ! A.role "dialog"
      ! customAttribute "aria-labelledby" (H.toValue $ name ++ "-title") $
      content

dialog name content =
  H.div ! A.class_ "c-dialog-context c-dialog-context" $ do
    H.div ! A.class_ "c-dialog-backdrop c-dialog-backdrop" $ ""
    H.div ! A.class_ "c-dialog c-dialog--medium" ! A.role "dialog"
      ! customAttribute "aria-labelledby" (H.toValue $ name ++ "-title") $
      content

dialogVisible name content =
  H.div ! A.class_ "c-dialog-context c-dialog-context--visible" $ do
    H.div ! A.class_ "c-dialog-backdrop c-dialog-backdrop--visible" $ ""
    H.div ! A.class_ "c-dialog c-dialog--medium" ! A.role "dialog"
      ! customAttribute "aria-labelledby" (H.toValue $ name ++ "-title") $
      content

dialogContent name = do
      H.div ! A.class_ "c-dialog__push" $ ""
      H.div ! A.class_ "c-dialog__header c-dialog__header--bordered" $
        H.div ! A.class_ "c-toolbar c-toolbar--spaced" $ do
          H.div ! A.class_ "c-toolbar__left" $
            H.div ! A.class_ "c-toolbar__item" $
              H.h2 ! A.class_ "c-dialog__title"
                ! A.id (H.toValue $ name ++ "-title") $ "Dialog title"
          H.div ! A.class_ "c-toolbar__item" $
            H.button ! A.class_ "c-button c-button--borderless c-button--icon"
              ! customAttribute "data-dialog-close aria-label ""Close dialog" $
              H.div ! A.class_ "c-button__content" $
                H.div ! A.class_ "o-svg-icon o-svg-icon-close  " $
                  svgIconClose
      H.div ! A.class_ "c-dialog__body" $
        H.div ! A.class_ "u-spacer-l" $
          H.div ! A.class_ "o-form-group-layout o-form-group-layout--standard" $ do
            H.div ! A.class_ "o-form-group" $ do
              H.label ! A.class_ "o-form-group__label" ! A.for "input" $ "Input"
              H.div ! A.class_ "o-form-group__controls" $
                H.input ! A.class_ "c-input" ! A.type_ "text" ! A.id "input"
            H.div ! A.class_ "o-form-group" $ do
              H.label ! A.class_ "o-form-group__label" ! A.for "select" $ "Select"
              H.div ! A.class_ "o-form-group__controls" $ do
                H.div ! A.class_ "c-select-holder" $
                  H.select ! A.class_ "c-select" ! A.id "select" $
                    mapM_ (H.option . H.toHtml)
                      ([ "Choose an item", "A", "B", "C" ] :: [String])
                H.p ! A.class_ "c-form-help-text" $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed commodo accumsan risus."
            H.div ! A.class_ "o-form-group" $ do
              H.label ! A.class_ "o-form-group__label" ! A.for "textarea" $ "Textarea"
              H.div ! A.class_ "o-form-group__controls" $ do
                H.textarea ! A.class_ "c-textarea" ! A.rows "5" ! A.id "textarea" $ ""
                H.p ! A.class_ "c-form-help-text" $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed commodo accumsan risus."
      H.div ! A.class_ "c-dialog__footer c-dialog__footer--bordered" $
        H.div ! A.class_ "c-toolbar c-toolbar--spaced" $ do
          H.div ! A.class_ "c-toolbar__left" $
            H.div ! A.class_ "c-toolbar__item" $ ""
          H.div ! A.class_ "c-toolbar__right" $
            H.div ! A.class_ "c-toolbar__item" $
              H.div ! A.class_ "c-button-toolbar" $ do
                H.button ! A.class_ "c-button c-button--secondary"
                  ! customAttribute "data-dialog-close" "close" $
                  H.div ! A.class_ "c-button__content" $
                    H.div ! A.class_ "c-button__label" $ "Cancel"
                H.button ! A.class_ "c-button c-button--primary"
                  ! customAttribute "data-dialog-close" "close" $
                  H.div ! A.class_ "c-button__content" $
                    H.div ! A.class_ "c-button__label" $ "Save"
      H.div ! A.class_ "c-dialog__push" $ ""
