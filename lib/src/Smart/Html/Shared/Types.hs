module Smart.Html.Shared.Types
  ( Title(..)
  ) where

import qualified Text.Blaze.Html5              as H

-- | Title of a newtype, the IsString instance provides us with convenience on using
-- overloaded string literals as `Title` values on inference.
newtype Title = Title { _unTitle :: Text }
              deriving (Eq, Show, IsString, H.ToMarkup) via Text

