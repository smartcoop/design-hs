{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Smart.Html.Avatar
  ( AvatarSize(..)
  , AvatarAdditionalContent(..)
  , Avatar(..)
  ) where

import qualified Data.Text                     as T
import qualified Smart.Html.Shared.Html.Icons  as Icons
import           Smart.Html.Shared.Types        ( URI(..) )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

data AvatarSize = Small | Regular
                deriving (Eq, Show)

-- | An avatar image, if present.
data AvatarImage = AvatarImage URI
                 | NoAvatarImage
                 deriving (Eq, Show)

-- | For images, we want to draw the svg-icon indicating a user if no image is present; or the image itself, if it is present.
instance H.ToMarkup AvatarImage where
  toMarkup = \case
    AvatarImage (URI uri) -> H.img ! A.src (H.textValue uri) ! A.alt "avatar"
    NoAvatarImage         -> H.toMarkup Icons.svgIconUser

-- | Additional avatar content: either text or the initials of the user. 
data AvatarAdditionalContent =
  -- | An avatar with initials.
   AvatarInitials Text
  | AvatarText Text
  | NoAdditionalContent
  deriving Show

data Avatar = Avatar AvatarImage AvatarSize AvatarAdditionalContent
            | AvatarLink AvatarImage URI AvatarSize AvatarAdditionalContent

instance H.ToMarkup Avatar where
  toMarkup = \case
    Avatar img size additionalContent ->
      drawAvatarUnder H.div img size additionalContent
    AvatarLink img (URI (H.textValue -> link')) size additionalContent ->
      drawAvatarUnder (H.a ! A.href link') img size additionalContent
   where
    drawAvatarUnder container img size additionalContent =
      case additionalContent of
        AvatarText txt -> H.div ! A.class_ "c-avatar-and-text" $ do
          drawAvatarUnder container img size NoAdditionalContent
          H.div ! A.class_ "c-avatar-and-text__text" $ H.p $ H.toMarkup txt
        otherContent ->
          container
            !  A.class_ (avatarClass img size)
            $  H.toMarkup img
            >> H.toMarkup otherContent
    avatarClass img size =
      H.textValue . T.intercalate " " $ ["c-avatar", sizeC, imgC]
     where
      sizeC = case size of
        Small   -> "c-avatar--small"
        Regular -> "c-avatar--regular"

      imgC = case img of
        NoAvatarImage -> mempty
        AvatarImage{} -> "c-avatar--img"

instance H.ToMarkup AvatarAdditionalContent where
  toMarkup = \case
    AvatarInitials initials -> H.toMarkup . T.toUpper $ initials
    AvatarText     text     -> H.toMarkup text
    NoAdditionalContent     -> mempty


