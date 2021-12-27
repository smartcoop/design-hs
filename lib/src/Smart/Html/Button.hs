{- |
Module: Smart.Html.Button
Description: Buttons

FIXME: Add support for dynamic behaviour attributes like "data-alert-close" etc.

-}
module Smart.Html.Button
  ( Button(..)
  , ButtonDef(..)
  ) where

import           Smart.Html.Shared.Html.Icons
import           Smart.Html.Shared.Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | Sum type to indicate what kind of button to include in the components. 
data ButtonDef =
  -- | Use a default close button 
  CloseButton
  -- | Use no buttons in the component. 
  | NoButton
  -- | Use a custom button in the component. 
  | CustomButton Button

instance H.ToMarkup ButtonDef where
  toMarkup = \case
    CustomButton btn -> H.toMarkup btn
    CloseButton      -> H.toMarkup $ ButtonIcon svgIconClose
    NoButton         -> mempty

-- | Buttons are much simpler: some of the button variants can be enabled or disabled.
data Button where
  -- | A primary button with a title, can be disabled.
  ButtonPrimary ::Title -> ElemEnabledState -> Button
  -- | A secondary button with a title, can be disabled.
  ButtonSecondary ::Title -> ElemEnabledState -> Button
  -- | A danger secondary-button, cannot be disabled.
  ButtonDangerSecondary ::Title -> Button
  -- | A danger button, cannot be disabled.
  ButtonDanger ::Title -> Button
  -- | Borderless button (just has text)
  ButtonBorderless ::Title -> Button
  -- | A button with an icon and text, the icon is on the left. 
  ButtonSecondaryIconAndText ::Icon -> Title -> Button
  -- | A button with text and an icon, the icon is on the right.
  ButtonSecondaryTextAndIcon ::Title -> Icon -> Button
  -- | A button with just an icon.
  ButtonIcon ::Icon -> Button
  ButtonIconBorderless ::KnownSymbol iconType
                       => OSvgIconDiv iconType -- ^ Button Icon 
                       -> Maybe Title -- ^ Maybe the button text for accessiblity.
                       -> Button

instance H.ToMarkup Button where

  toMarkup = \case
    ButtonPrimary title en -> mkButton @Icon (Just en)
                                             Nothing
                                             (Just title)
                                             Nothing
                                             "c-button c-button--primary"
    ButtonSecondary title en -> mkButton @Icon (Just en)
                                               Nothing
                                               (Just title)
                                               Nothing
                                               "c-button c-button--secondary"
    ButtonDangerSecondary title -> mkButton @Icon
      Nothing
      Nothing
      (Just title)
      Nothing
      "c-button c-button--danger-secondary"
    ButtonDanger title -> mkButton @Icon Nothing
                                         Nothing
                                         (Just title)
                                         Nothing
                                         "c-button c-button--danger"
    ButtonBorderless title -> mkButton @Icon Nothing
                                             Nothing
                                             (Just title)
                                             Nothing
                                             "c-button c-button--borderless"
    ButtonSecondaryIconAndText icon title -> mkButton @Icon
      Nothing
      (Just icon)
      (Just title)
      Nothing
      "c-button c-button--secondary"
    ButtonSecondaryTextAndIcon title icon -> mkButton @Icon
      Nothing
      Nothing
      (Just title)
      (Just icon)
      "c-button c-button--secondary"
    ButtonIcon icon -> mkButton @Icon Nothing
                                      Nothing
                                      Nothing
                                      (Just icon)
                                      "c-button c-button--secondary"
    ButtonIconBorderless icon mTitle -> mkButton
      Nothing
      (Just icon)
      mTitle
      Nothing
      "c-button-link c-button--borderless-muted c-button--icon"

-- | A generalised mkButton, since most buttons have the same HTML.
mkButton
  :: forall icon
   . H.ToMarkup icon
  => Maybe ElemEnabledState
  -> Maybe icon
  -> Maybe Title
  -> Maybe Icon
  -> H.AttributeValue -- ^ Button class. 
  -> H.Html
mkButton mEnabled mIconL mTitle mIconR buttonClass =
  maybe identity elemEnabledStateAttr mEnabled
    .  (H.button ! A.class_ buttonClass ! A.type_ "button")
    $  iconL
    *> span
    <* iconR
 where
  span =
    maybe mempty ((H.span ! A.class_ "c-button__content") . H.toMarkup) mTitle
  iconL = maybe mempty H.toMarkup mIconL
  iconR = maybe mempty H.toMarkup mIconR
