{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Smart.Html.FileAttachment
  ( FileAttachment(..)
  , FileAttachmentList(..)
  ) where

import qualified Smart.Html.Shared.Html.Icons  as Icons
import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | File attachments, refer to the <https://design.smart.coop/development/docs/c-file-attachment.html docs>.
data FileAttachment = FileAttachment Types.Title Types.URI

instance H.ToMarkup FileAttachment  where
  toMarkup (FileAttachment title (Types.URI uri)) =
    (H.a ! A.href (H.textValue uri)) $ iconDivM >> titleM
   where
    iconDivM = H.toMarkup
      $ Icons.IconDiv @"c-file-attachment" Icons.svgIconFileAttachment
    titleM = H.span ! A.class_ "c-file-attachment__name" $ H.toMarkup title

newtype FileAttachmentList = FileAttachmentList [ FileAttachment ]

instance H.ToMarkup FileAttachmentList where
  toMarkup (FileAttachmentList fas) =
    (H.ul ! A.class_ "c-file-attachment-list")
      .   mconcat
      $   mkFileAttachmentLi
      <$> fas
   where
    mkFileAttachmentLi = (H.li ! A.class_ "c-file-attachment") . H.toMarkup
