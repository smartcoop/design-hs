module Examples.FileAttachment
  ( fileAttachments
  ) where

import           Smart.Html.FileAttachment
import           Smart.Html.Shared.Types        ( Title(..) )

fileAttachments :: [FileAttachmentList]
fileAttachments =
  replicate 3 . FileAttachmentList $ mkFileAttachment <$> [0 .. 2]
 where
  mkFileAttachment idx =
    let fileName = Title $ "File name: " <> show @Int idx
    in  FileAttachment fileName "https://google.com"
