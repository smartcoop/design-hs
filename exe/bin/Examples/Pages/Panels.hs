-- Various "panels" or similar.
module Examples.Pages.Panels
  ( panelsPage
  ) where

import           Smart.Html.Shared.Html.Icons
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- See https://design.smart.coop/prototypes/old-desk/contract-create-5-alt.html
panelsPage = wrapper $ panels [panel1, panel2, panel3, panel4]

-- whole page
wrapper content =
  H.div
    ! A.class_ "c-app-layout"
    $ H.main
    ! A.class_ "u-maximize-width u-scroll-wrapper"
    $ content

-- all the panels
panels xs =
  H.div
    ! A.class_ "u-scroll-wrapper-body"
    $ H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container o-container--medium"
    $ mapM_ (H.div ! A.class_ "u-padding-vertical-l") xs

panel title editable content = H.div ! A.class_ "c-panel" $ do
  H.div ! A.class_ "c-panel__header" $ H.div ! A.class_ "c-toolbar" $ do
    H.div
      ! A.class_ "c-toolbar__left"
      $ H.h2
      ! A.class_ "c-panel__title"
      $ title
    if editable
      then
        H.div
        ! A.class_ "c-toolbar__right"
        $ H.a
        ! A.class_ "c-button c-button--secondary"
        ! A.href "#"
        $ H.span
        ! A.class_ "c-button__content"
        $ do
            H.div ! A.class_ "o-svg-icon o-svg-icon-edit" $ H.toMarkup
              svgIconEdit
            H.span ! A.class_ "c-button__label" $ "Edit"
      else mempty
  H.div ! A.class_ "c-panel__body" $ content

keyValues xs =
  H.dl ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short" $ do
    mapM_ (uncurry keyValue) xs

keyValue k v = H.div ! A.class_ "c-key-value-item" $ do
  H.dt ! A.class_ "c-key-value-item__key" $ k
  H.dd ! A.class_ "c-key-value-item__value" $ v

panel1 = panel "General information" True $ keyValues
  [ ("Worker", "Manfred")
  , ( "Work setting"
    , "The work is mainly performed in places and at times freely chosen"
    )
  , ("Compensation budget", "1000.00 EUR")
  , ("Project"            , "Unspecified")
  , ("Description"        , "Some description.")
  ]

panel2 = panel "Location and dates" True $ do
  keyValues [("Work country", "Belgium"), ("Work dates", "2022 January 7")]

  H.div
    ! A.class_ "u-spacer-top"
    $ H.div
    ! A.class_ "o-form-group"
    $ H.div
    ! A.class_ "o-form-group__controls o-form-group__controls--full-width"
    $ H.div
    ! A.class_ "c-checkbox"
    $ H.label
    $ do
        H.input ! A.type_ "checkbox"
        "By ticking this checkbox, you confirm COVID-19 dolore \
            \excepteur excepteur consequat ullamco dolore consequat."

panel3 = panel "Role and risks" True $ keyValues
  [ ("Role", "Art and craft creation > Entertainment arts > Dancer")
  , ("Work type" , "Artistic")
  , ("Work risks", "None")
  ]

panel4 = panel "Contract type" True $ keyValues
  [ ("Contract type", "Artistic contract, paid based on duration")
  , ("Is your fiscal residence abroad", "No")
  , ("Withholding rate"               , "11.11%")
  , ("Daily schedule"                 , "Full time")
  ]
