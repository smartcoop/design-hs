module Smart.Html.Input
  ( TextInput(..)
  ) where

import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | A simple text input. 
data TextInput = PlainTextInput Types.ElemEnabledState Types.Id Types.Name (Maybe Text)
               | PasswordInput Types.ElemEnabledState Types.Id Types.Name (Maybe Text)
               deriving (Eq, Show)

instance H.ToMarkup TextInput where
  toMarkup = \case
    PlainTextInput state' id name mval -> mkInput state' "text" id name mval
    PasswordInput  state' id name mval -> mkInput state' "password" id name mval
   where
    mkInput state' type' id (Types.Name name) mval =
      Types.elemEnabledStateAttr state'
        $ H.input
        ! A.type_ type'
        ! A.class_ "c-input"
        ! addValue
        ! Types.id id
        ! A.name (H.textValue name)
      where addValue = maybe mempty (A.value . H.textValue) mval
