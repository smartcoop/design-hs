module Examples.FileUpload
  ( fileUploads
  , fileUploadResults
  ) where

import           Smart.Html.Button
import           Smart.Html.FileUpload
import qualified Smart.Html.Shared.Html.Icons  as Icons

fileUploads :: [FileUpload]
fileUploads =
  [ ViaButtonOrDragIn
    (ButtonSecondaryIconAndText Icons.svgIconAdd "Select file")
    "Or drag a file into this area."
  , ViaDragIn "Drag a file into this area."
  ]

fileUploadResults :: [FileUploadResult]
fileUploadResults =
  [ Uploading @Text "Uploading your file..." (FileUploadProgress 0.8)
  , UploadSuccess @Text "Done!"
  , UploadFailure @Text "Failed to upload files." (Just retryButton)
  ]
  where retryButton = ButtonDangerSecondary "Retry"
