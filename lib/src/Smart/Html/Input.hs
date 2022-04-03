module Smart.Html.Input
  ( TextInput(..)
  ) where

import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | A simple text input. 
data TextInput = PlainTextInput Types.ElemEnabledState Types.Id (Maybe Text)
               | PasswordInput Types.ElemEnabledState Types.Id (Maybe Text)
               deriving (Eq, Show)

instance H.ToMarkup TextInput where
  toMarkup = \case
    PlainTextInput state' id mval -> mkInput state' "text" id mval
    PasswordInput  state' id mval -> mkInput state' "password" id mval
   where
    mkInput state' type' id mval =
      Types.elemEnabledStateAttr state'
        $ H.input
        ! A.type_ type'
        ! addValue
        ! Types.id id
      where addValue = maybe mempty (A.value . H.textValue) mval
