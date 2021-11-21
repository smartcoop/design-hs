module Smart.Html.Shared.Types
  ( ElemEnabledState(..)
  , elemEnabledStateAttr
  , Title(..)
  , Id(..)
  , id
  , Name(..)
  , Body(..)
  , Placeholder'(..)
  , BodyPlaceholder
  -- Confirmation buttons 
  , ConfirmText(..)
  , CancelText(..)
  , URI(..)
  , Image(..)
  , Rows(..)
  , rows
  ) where

import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

-- | ID of an HTML element.
newtype Id = Id { _unId :: Text }
           deriving (Eq, Show, IsString, Semigroup, Monoid) via Text

-- | Attach an @id@ attribute using our @Id@ newtype 
id :: Id -> H.Attribute
id = H.customAttribute "id" . H.textValue . _unId

-- | Enabled state of an element 
data ElemEnabledState = Enabled | Disabled
                      deriving (Eq, Show)

-- | Apply the ElemEnabledState to an actual Html element.
elemEnabledStateAttr :: ElemEnabledState -> H.Html -> H.Html
elemEnabledStateAttr en html = case en of
  Enabled  -> html
  Disabled -> html H.! A.disabled "disabled"

-- | Title of a newtype, the IsString instance provides us with convenience on using
-- overloaded string literals as `Title` values on inference.
newtype Title = Title { _unTitle :: Text }
              deriving (Eq, Show, IsString, H.ToMarkup) via Text

-- | Name of a newtype, the IsString instance provides us with convenience on using
-- overloaded string literals as `Name` values on inference.
newtype Name = Name { _unName :: Text }
              deriving (Eq, Show, IsString, H.ToMarkup) via Text


-- | Body of a newtype, the IsString instance provides us with convenience on using
-- overloaded string literals as `Body` values on inference.
newtype Body = Body { _unBody :: Text }
              deriving (Eq, Show, IsString, H.ToMarkup) via Text

newtype Placeholder' body = Placeholder { _unBodyPlaceholder :: body }
                        deriving (Eq, Show, IsString, H.ToMarkup) via body

type BodyPlaceholder = Placeholder' Body

-- | Confirmation text: eg. for use in a dialog confirming an action
newtype ConfirmText = ConfirmText { _unConfirmText :: Text }
              deriving (Eq, Show, IsString, H.ToMarkup) via Text

-- | Cancellation text: eg. for use in a dialog aborting an action
newtype CancelText = CancelText { _unCancelText :: Text }
              deriving (Eq, Show, IsString, H.ToMarkup) via Text

-- | A newtype around a URI, which is, at this stage, just represented by a text.
newtype URI = URI { _unURI :: Text }
            deriving (Eq, Show, IsString, H.ToMarkup) via Text

-- | A newtype around a Image, which is, at this stage, just represented by a text.
newtype Image = Image { _unImage :: URI }
            deriving (Eq, Show, IsString, H.ToMarkup) via URI

-- | A @rows@ attribute, can be used in a @textarea@.
newtype Rows = Rows { _unRows :: Int }
             deriving (Eq, Show, Ord, Real, Enum, Integral, Num) via Int

-- | Attach a @rows@ attribute using our @Rows@ newtype.
rows :: Rows -> H.Attribute
rows = A.rows . H.textValue . show . _unRows
