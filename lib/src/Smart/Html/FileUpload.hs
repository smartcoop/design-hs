module Smart.Html.FileUpload
  ( FileUpload(..)
  , FileUploadResult(..)
  , FileUploadProgress(..)
  ) where

import           Smart.Html.Button
import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | Element to upload files. 
data FileUpload =
  -- | File upload with a body and a button to upload files. 
  ViaButtonOrDragIn Button Types.Body
  -- | A file upload with just a body text (drag and drop files)
  | ViaDragIn Types.Body

-- | File uploads are always nested under a root @div@, say D, with at least "c-file-upload" as its class.
-- Inside D, there's always an @input@, I, with @type=file@.
-- Alongside I, we have another @div@, E, which may contain a button + text, or just text.
instance H.ToMarkup FileUpload where
  toMarkup = \case
    ViaButtonOrDragIn button body ->
      mkFileUpload Types.Enabled body Nothing (Just $ H.toMarkup button)
    ViaDragIn body ->
      mkFileUpload Types.Enabled body (Just "c-file-upload--gray-25") Nothing

mkFileUpload
  :: forall body
   . H.ToMarkup body
  => Types.ElemEnabledState -- ^ Enabled/Disabled state of the input.
  -> body -- ^ Content body
  -> Maybe H.AttributeValue -- ^ Additional top-level class. 
  -> Maybe H.Html -- ^ Additional content. 
  -> H.Html
mkFileUpload inputDisabled body mAdditionalClass mAdditionalM =
  let topLevelClass = "c-file-upload" <> maybe "" (" " <>) mAdditionalClass
  in  (H.div ! A.class_ topLevelClass) $ fileInputM >> contentM
 where
  fileInputM =
    H.input
      ! A.type_ "file"
      ! (if inputDisabled == Types.Disabled
          then A.disabled "disabled"
          else mempty
        )
  contentM =
    H.div
      !  A.class_ "c-file-upload__content"
      $  fromMaybe mempty mAdditionalM
      <> bodyM
  bodyM = H.p $ H.toMarkup body

-- | Indicates file upload progress. FIXME: Use of double not entirely safe, but it will do for now.
newtype FileUploadProgress = FileUploadProgress Double
                           deriving Show

instance H.ToMarkup FileUploadProgress where
  toMarkup (FileUploadProgress ratio) =
    H.div ! A.class_ class_ ! A.style style $ mempty
   where
    class_ =
      "c-progress-bar" <> if ratio >= 1 then " c-progress-bar--success" else ""
    style = H.textValue $ "--value: " <> show ratio

-- | The result of uploading files. 
data FileUploadResult where
  -- | Ongoing upload.
  Uploading ::H.ToMarkup status => { uploadingStatus :: status
                                   , uploadingProgress :: FileUploadProgress
                                   } -> FileUploadResult

  UploadSuccess ::H.ToMarkup status => { successStatus :: status } -> FileUploadResult

  UploadFailure ::H.ToMarkup status => { failureStatus :: status
                                       , failureRetryButton :: Maybe Button
                                       } -> FileUploadResult

instance H.ToMarkup FileUploadResult where
  toMarkup = \case

    Uploading status progress ->
      mkFileUploadStatus $ mkStatusM status >> H.toMarkup progress

    UploadSuccess status ->
      mkFileUploadStatus $ mkStatusM status >> H.toMarkup (FileUploadProgress 1)

    UploadFailure status mRetry ->
      mkFileUploadStatus $ mkStatusM status >> maybe mempty H.toMarkup mRetry

   where
    mkFileUploadStatus =
      mkFileUpload Types.Disabled (mempty @H.Html) Nothing . Just
    mkStatusM status =
      H.div ! A.class_ "c-file-upload__status" $ H.toMarkup status
