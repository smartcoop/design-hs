{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Smart.Html.Avatar
  ( AvatarSize(..)
  , AvatarImage(..)
  , AvatarAdditionalContent(..)
  , AvatarAdditionalContentType(..)
  , Avatar(..)
  ) where

import qualified Data.Text                     as T
import qualified Smart.Html.Shared.Html.Icons  as Icons
import           Smart.Html.Shared.Types        ( Body
                                                , Image(..)
                                                , Title
                                                , URI(..)
                                                )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

data AvatarSize = Small | Regular
                deriving (Eq, Show)

-- | An avatar image, if present.
data AvatarImage = AvatarImage Image
                 | NoAvatarImage
                 deriving (Eq, Show)

-- | For images, we want to draw the svg-icon indicating a user if no image is present; or the image itself, if it is present.
instance H.ToMarkup AvatarImage where
  toMarkup = \case
    AvatarImage (Image (URI uri)) ->
      H.img ! A.src (H.textValue uri) ! A.alt "avatar"
    NoAvatarImage -> H.toMarkup Icons.svgIconUser

-- | Additional avatar content: either text or the initials of the user. 
data AvatarAdditionalContent (contentType :: AvatarAdditionalContentType) where
  -- | An avatar with initials.
  AvInitials ::Text -> AvatarAdditionalContent 'WithInitials
  AvText ::Text -> AvatarAdditionalContent 'WithText
  AvTitleAndBody ::Title -> Maybe Body -> AvatarAdditionalContent 'WithTitleAndBody
  AvNoAdditionalContent ::AvatarAdditionalContent 'WithoutAdditionalContent

deriving instance Show (AvatarAdditionalContent ctype)

-- | A simple sum-type used at the kind level to indicate if an avatar datum has additional content or not at the type-level.
data AvatarAdditionalContentType = WithInitials
                                 | WithText
                                 | WithTitleAndBody
                                 | WithoutAdditionalContent
                                 deriving Show

-- | Avatar: parameterised at the kind level with the content-types.
-- This lets us observe type-safety for avatars and restricting guarantees on the additional content at the type level.
-- This is useful in cards, where we want Avatars to /not/ have any additional content.
data Avatar (contentType :: AvatarAdditionalContentType) where
  Avatar ::AvatarImage -> AvatarSize -> AvatarAdditionalContent contentType -> Avatar contentType
  AvatarLink ::AvatarImage -> URI -> AvatarSize -> AvatarAdditionalContent contentType -> Avatar contentType

deriving instance Show (Avatar ctype)

instance H.ToMarkup (Avatar contentType) where
  toMarkup = \case
    Avatar img size additionalContent ->
      drawAvatarUnder H.div img size (Just additionalContent)
    AvatarLink img (URI (H.textValue -> link')) size additionalContent ->
      drawAvatarUnder (H.a ! A.href link') img size (Just additionalContent)

drawAvatarUnder
  :: forall contentType
   . (H.Markup -> H.Markup)
  -> AvatarImage
  -> AvatarSize
  -> Maybe (AvatarAdditionalContent contentType)
  -> H.Markup
drawAvatarUnder container img size additionalContent =
  case additionalContent of
    Just textContent@AvText{} -> H.div ! A.class_ "c-avatar-and-text" $ do
      drawAvatarUnder container img size Nothing
      H.toMarkup textContent
    otherContent ->
      container
        !  A.class_ (avatarClass img size)
        $  H.toMarkup img
        >> maybe mempty H.toMarkup otherContent

avatarClass img size =
  H.textValue . T.intercalate " " $ ["c-avatar", sizeC, imgC]
 where
  sizeC = case size of
    Small   -> "c-avatar--small"
    Regular -> "c-avatar--regular"

  imgC = case img of
    NoAvatarImage -> mempty
    AvatarImage{} -> "c-avatar--img"

instance H.ToMarkup (AvatarAdditionalContent contentType) where
  toMarkup = \case
    AvInitials initials        -> H.toMarkup . T.toUpper $ initials

    AvText     text            -> avatarAndText . H.p $ H.toMarkup text
    AvTitleAndBody title mBody -> avatarAndText $ titleM >> bodyM
     where
      titleM = H.h4 ! A.class_ "c-h4" $ H.toMarkup title
      bodyM  = maybe mempty ((H.p ! A.class_ "u-text-muted") . H.toMarkup) mBody

    AvNoAdditionalContent -> mempty
    where avatarAndText = H.div ! A.class_ "c-avatar-and-text__text"



