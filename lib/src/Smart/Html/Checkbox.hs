module Smart.Html.Checkbox
  ( Checkbox(..)
  , CheckboxState(..)
  ) where

import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

data CheckboxState = Unchecked | Checked
                   deriving (Eq, Show)

-- | A single checkbox (not a group of checkboxes) 
data Checkbox =
  -- | An enabled checkbox with a given id and a body.
  CheckboxEnabled (Maybe Types.Id) CheckboxState Types.Body
  -- | A disabled checkbox with a given id and a body.
  | CheckboxDisabled (Maybe Types.Id) CheckboxState Types.Body
  deriving Show

instance H.ToMarkup Checkbox where
  toMarkup = \case
    CheckboxEnabled  mId cState body -> mkCheckbox True mId cState body
    CheckboxDisabled mId cState body -> mkCheckbox False mId cState body
   where
    mkCheckbox enabled mId cState body =
      (H.div ! A.class_ "c-checkbox") . H.label $ inputM >> H.toMarkup body
     where
      inputM    = H.input ! A.type_ "checkbox" ! mDisabled ! mChecked ! mIdA
      mDisabled = if enabled then mempty else A.disabled "disabled"
      mChecked  = if cState == Unchecked then mempty else A.checked "checked"
      mIdA      = maybe mempty Types.id mId

