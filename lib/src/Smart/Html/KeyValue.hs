module Smart.Html.KeyValue
  ( KeyValue(..)
  , KeyValueGroup(..)
  ) where

import           Smart.Html.Shared.Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | Each item of a key value group
-- Items here are rendered under @c-key-value-item@ classes. 
data KeyValue where
  KeyValue ::H.ToMarkup value => Title -> value -> KeyValue

instance H.ToMarkup KeyValue where
  toMarkup (KeyValue key value) =
    (H.div ! A.class_ "c-key-value-item") $ keyM >> valueM
   where
    keyM   = H.dt ! A.class_ "c-key-value-item__key" $ H.toMarkup key
    valueM = H.dd ! A.class_ "c-key-value-item__value" $ H.toMarkup value

-- | A group of KeyValues. 
newtype KeyValueGroup = KeyValueGroup [KeyValue]

instance H.ToMarkup KeyValueGroup where
  toMarkup (KeyValueGroup kvs) =
    (H.dl ! A.class_ "c-key-value") . mconcat $ H.toMarkup <$> kvs

