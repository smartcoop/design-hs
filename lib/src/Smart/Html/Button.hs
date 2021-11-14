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
data Button =
  -- | A primary button with a title, can be disabled.
  ButtonPrimary Title ElemEnabledState
  -- | A secondary button with a title, can be disabled.
  | ButtonSecondary Title ElemEnabledState
  -- | A danger secondary-button, cannot be disabled.
  | ButtonDangerSecondary Title
  -- | A danger button, cannot be disabled.
  | ButtonDanger Title
  -- | Borderless button (just has text)
  | ButtonBorderless Title
  -- | A button with an icon and text, the icon is on the left. 
  | ButtonSecondaryIconAndText Icon Title
  -- | A button with text and an icon, the icon is on the right.
  | ButtonSecondaryTextAndIcon Title Icon
  -- | A button with just an icon.
  | ButtonIcon Icon

instance H.ToMarkup Button where

  toMarkup = \case
    ButtonPrimary title en ->
      mkButton (Just en) Nothing (Just title) Nothing "c-button--primary"
    ButtonSecondary title en ->
      mkButton (Just en) Nothing (Just title) Nothing "c-button--secondary"
    ButtonDangerSecondary title ->
      mkButton Nothing Nothing (Just title) Nothing "c-button--danger-secondary"
    ButtonDanger title ->
      mkButton Nothing Nothing (Just title) Nothing "c-button--danger"
    ButtonBorderless title ->
      mkButton Nothing Nothing (Just title) Nothing "c-button--borderless"
    ButtonSecondaryIconAndText icon title ->
      mkButton Nothing (Just icon) (Just title) Nothing "c-button--secondary"
    ButtonSecondaryTextAndIcon title icon ->
      mkButton Nothing Nothing (Just title) (Just icon) "c-button--secondary"
    ButtonIcon icon ->
      mkButton Nothing Nothing Nothing (Just icon) "c-button--secondary"

-- | A generalised mkButton, since most buttons have the same HTML.
mkButton
  :: Maybe ElemEnabledState
  -> Maybe Icon
  -> Maybe Title
  -> Maybe Icon
  -> H.AttributeValue
  -> H.Html
mkButton mEnabled mIconL mTitle mIconR specificClass =
  maybe identity elemEnabledStateAttr mEnabled
    $ H.button
    ! A.class_ ("c-button " <> specificClass)
    ! A.type_ "button"
    $ (iconL *> span <* iconR)
 where
  span =
    maybe mempty ((H.span ! A.class_ "c-button__content") . H.toMarkup) mTitle
  iconL = maybe mempty H.toMarkup mIconL
  iconR = maybe mempty H.toMarkup mIconR
