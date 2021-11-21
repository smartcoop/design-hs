module Smart.Html.Loader
  ( Loader(..)
  ) where

import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

newtype Loader = ThreeBouncingDots (Maybe Types.Id)
  deriving (Eq, Show)

instance H.ToMarkup Loader where
  toMarkup (ThreeBouncingDots mId) =
    ( H.div
      ! A.class_ "c-loader"
      ! A.role "alert"
      ! H.customAttribute "aria-busy" "true"
      ! id
      )
      mempty
    where id = maybe mempty (A.id . H.textValue . Types._unId) mId
