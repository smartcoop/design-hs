{- |
Module: Smart.Html.Accordion
Description: The accordion of the Smart design system.

See <https://design.smart.coop/development/docs/c-accordion.html docs>
-}
module Smart.Html.Accordion
  () where

import qualified Smart.Html.Dsl.Class          as Dsl
import qualified Smart.Html.Shared.Html.Helpers
                                               as Helpers
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

  <div class="c-accordion">
    <div class="c-accordion__item">
        <div class="c-accordion__item-header">
            <div class="c-toolbar">
                <div class="c-toolbar__left">
                    <div class="c-toolbar__item">
-}

instance H.ToMarkup Accordion where
  toMarkup = \case 
    Accordion items -> 
      Helpers.multiNestedClassedElems H.div [ "c-accordion", "c-accordion__item" , "c-accordion__item-header", "c-toolbar", "c-toolbar__left" , "c-toolbar__item" ] undefined 
    where
      drawItems :: [AccordionItem] -> H.Html 
      drawItems = undefined 
    
instance H.ToMarkup AccordionItem where
