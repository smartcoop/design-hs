{- |
Module: Smart.Html.Accordion
Description: The accordion of the Smart design system.

See <https://design.smart.coop/development/docs/c-accordion.html docs>
-}
module Smart.Html.Accordion
  ( Accordion(..)
  , AccordionItem(..)
  ) where

import qualified Smart.Html.Shared.Html.Helpers
                                               as Helpers
import           Smart.Html.Shared.Html.Icons  as Icons
import           Smart.Html.Shared.Types
import qualified Text.Blaze.Html5              as H

data Accordion where

  -- | A simple accordion.
  Accordion ::[AccordionItem] -> Accordion

  -- | An accordion of type Action.
  AccordionAction ::[AccordionItem] -> Accordion

-- | An accordion item has some arbitrary HTML content; and a title.
data AccordionItem where
  (:>) ::H.ToMarkup content => Title -> content -> AccordionItem

infixr 5 :>

------------------------------------
-- Instances for generating HTML  --
------------------------------------

{- $exampleHtml
Example accordion's initial divs:

@
<div class="c-accordion">
  <div class="c-accordion__item">
      <div class="c-accordion__item-header">
          <div class="c-toolbar">
              <div class="c-toolbar__left">
                  <div class="c-toolbar__item">
@

TODO: also implement for AccordionAction

-}
instance H.ToMarkup Accordion where
  toMarkup = \case
    Accordion items -> H.div . mconcat $ H.toMarkup <$> items

instance H.ToMarkup AccordionItem where
  toMarkup (titleT :> cnt) =
    Helpers.classedElem H.div ["c-accordion__item"] $ header >> content
   where
    content = H.toMarkup cnt
    header =
      Helpers.multiNestedClassedElems
          H.div
          [ "c-accordion__item-header"
          , "c-toolbar"
          , "c-toolbar__left"
          , "c-toolbar__item"
          ]
        $  button
        >> title

    button =
      Helpers.classedElem
          H.button
          ["c-button", "c-button--borderless", "c-button-icon"]
        .  Helpers.classedElem H.span ["c-button__content"]
        $  svgIcon
        >> chevron
    svgIcon = Helpers.classedElem H.div
                                  ["o-svg-icon", "o-svg-icon-chevron-right"]
                                  Icons.svgIconChevronLeft -- TODO check if this is correct?
    chevron = Helpers.classedElem H.div ["u-sr-accessible"] "Chevron"

    title   = Helpers.classedElem H.span ["c-accordion__item-title"]
      $ H.toMarkup titleT

