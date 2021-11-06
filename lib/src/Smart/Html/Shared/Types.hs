module Smart.Html.Shared.Types
  ( ElemEnabledState(..)
  , elemEnabledStateAttr
  , Title(..)
  , Body(..)
  -- Confirmation buttons 
  , ConfirmText(..)
  , CancelText(..)
  ) where

import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

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

-- | Body of a newtype, the IsString instance provides us with convenience on using
-- overloaded string literals as `Body` values on inference.
newtype Body = Body { _unBody :: Text }
              deriving (Eq, Show, IsString, H.ToMarkup) via Text

-- | Confirmation text: eg. for use in a dialog confirming an action
newtype ConfirmText = ConfirmText { _unConfirmText :: Text }
              deriving (Eq, Show, IsString, H.ToMarkup) via Text

-- | Cancellation text: eg. for use in a dialog aborting an action
newtype CancelText = CancelText { _unCancelText :: Text }
              deriving (Eq, Show, IsString, H.ToMarkup) via Text
