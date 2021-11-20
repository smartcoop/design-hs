module Smart.Html.Form
  ( FormGroup(..)
  ) where

import qualified Data.Text                     as T
import qualified Smart.Html.Checkbox           as C
import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | Group of form inputs. 
data FormGroup =
  CheckboxGroup Types.Title [C.Checkbox]
  | CheckboxGroupInline Types.Title [C.Checkbox]
  deriving Show

instance H.ToMarkup FormGroup where
  toMarkup = \case
    CheckboxGroup title checkboxes -> drawCheckboxes Nothing title checkboxes
    CheckboxGroupInline title checkboxes ->
      drawCheckboxes (Just "c-checkbox-group--inline") title checkboxes
   where
    drawCheckboxes mExtraClass title checkboxes =
      mkFormGroup title
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

{- | Form groups are always rendered under the root tags:

@ 
<div class="o-form-group-layout o-form-group-layout--standard">
    <div class="o-form-group">
        <label class="o-form-group__label">Value</label>
        <div class="o-form-group__controls">
@

This function achieves that. 
-}
mkFormGroup title controls =
  (H.div ! A.class_ "o-form-group-layout o-form-group-layout--standard")
    .  (H.div ! A.class_ "o-form-group")
    $  labelM
    >> controlsM
 where
  labelM    = H.label ! A.class_ "o-form-group__label" $ H.toMarkup title
  controlsM = H.div ! A.class_ "o-form-group__controls" $ controls

