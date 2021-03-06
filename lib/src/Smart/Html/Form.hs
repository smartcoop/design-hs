module Smart.Html.Form
  ( FormGroup(..)
  ) where

import           Control.Lens
import qualified Data.Text                     as T
import qualified Smart.Html.Checkbox           as C
import qualified Smart.Html.Input              as Inp
import qualified Smart.Html.Shared.Types       as Types
import qualified Smart.Html.Textarea           as TA
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | Group of form inputs. 
data FormGroup =
  CheckboxGroup Types.Title [C.Checkbox]
  | CheckboxGroupInline Types.Title [C.Checkbox]
  -- | A group of text area. 
  | TextareaGroup [(Types.Title, TA.Textarea)]
  | InputGroup [(Types.Title, Inp.TextInput)]
  deriving Show

instance H.ToMarkup FormGroup where
  toMarkup = \case
    CheckboxGroup title checkboxes -> drawCheckboxes Nothing title checkboxes
    CheckboxGroupInline title checkboxes ->
      drawCheckboxes (Just "c-checkbox-group--inline") title checkboxes
    TextareaGroup labelsAndElems ->
      underFormGroupHeaders
        .   mconcat
        $   uncurry (labelAndInput . preview $ TA._Textarea . _2)
        <$> labelsAndElems
    InputGroup labelsAndInputs ->
      underFormGroupHeaders
        .   mconcat
        $   uncurry (labelAndInput getInputId)
        <$> labelsAndInputs
     where
      getInputId = Just . \case
        Inp.PlainTextInput _ id _ _ -> id
        Inp.PasswordInput  _ id _ _ -> id

   where
    drawCheckboxes mExtraClass title checkboxes =
      mkFormGroupGenericLabelUnderControls title
        $   (H.div ! A.class_ class_)
        .   mconcat
        $   H.toMarkup
        <$> checkboxes
     where
      class_ =
        H.textValue
          . T.unwords
          . catMaybes
          $ [Just "c-checkbox-group", mExtraClass]

labelAndInput
  :: forall input
   . H.ToMarkup input
  => (input -> Maybe Types.Id)
  -> Types.Title
  -> input
  -> H.Markup
labelAndInput getId title input =
  let id = getId input in labelM id title >> H.toMarkup input

-- | Generate a form-group without the label being /for/ a single input in the group of inputs.
mkFormGroupGenericLabelUnderControls title controls =
  underFormGroupHeaders $ labelM Nothing title >> controlsM
  where controlsM = H.div ! A.class_ "o-form-group__controls" $ controls

labelM mLabelForId =
  (H.label ! A.class_ "o-form-group__label" ! forId) . H.toMarkup
  where forId = maybe mempty (A.for . H.textValue . Types._unId) mLabelForId

{- | Form groups are always rendered under the root tags:

@ 
<div class="o-form-group-layout o-form-group-layout--standard">
    <div class="o-form-group">
        <label class="o-form-group__label">Value</label>
        <div class="o-form-group__controls">
@

This function achieves that. 
-}
underFormGroupHeaders =
  (H.div ! A.class_ "o-form-group-layout o-form-group-layout--standard")
    . (H.div ! A.class_ "o-form-group")
