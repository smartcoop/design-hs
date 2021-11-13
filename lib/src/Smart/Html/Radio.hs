module Smart.Html.Radio
  ( RadioButton(..)
  , RadioGroup(..)
  ) where

import           Smart.Html.Shared.Types        ( Name(..)
                                                , Title
                                                )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | A singleton radio button.
data RadioButton = RadioButton Title Name
  deriving Show

-- | A group of radio buttons, buttons can be grouped inline or on newlines. 
data RadioGroup =
  RadioGroup Title [RadioButton]
  | RadioGroupInline Title [RadioButton]
  deriving Show

instance H.ToMarkup RadioGroup where
  toMarkup = \case
    RadioGroup title buttons -> mkGroup title Nothing buttons
    RadioGroupInline title buttons ->
      mkGroup title (Just "c-radio-group--inline") buttons
   where
    mkGroup title mExtraClass buttons =
      (H.div ! A.class_ "o-form-group-layout o-form-group-layout--standard")
        $ (H.div ! A.class_ "o-form-group")
        $ do
            H.label ! A.class_ "o-form-group__label" $ H.toMarkup title
            (H.div ! A.class_ "o-form-group__controls")
              . (H.div ! A.class_ groupClass)
              $ mconcat (H.toMarkup <$> buttons)
      where groupClass = "c-radio-group" <> maybe "" (" " <>) mExtraClass

instance H.ToMarkup RadioButton where
  toMarkup (RadioButton title (Name name)) =
    H.div ! A.class_ "c-radio" $ H.label (radioInput >> H.toMarkup title)
    where radioInput = H.input ! A.type_ "radio" ! A.name (H.textValue name)
