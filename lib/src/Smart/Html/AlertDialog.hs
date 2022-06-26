{-|
Module: Smart.Html.AlertDialog
Description: Alert dialogs

<https://design.smart.coop/development/docs/c-alert-dialog.html Smart design website docs>
-}
module Smart.Html.AlertDialog
  ( AlertDialog(..)
  ) where

import qualified Smart.Html.Shared.Html.Helpers
                                               as Helpers
import           Smart.Html.Shared.Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- brittany-disable-next-binding 
-- | An alert dialog: comes in 2 flavours, one providing a generalised footer (for as many buttons as needed)
-- and the CancelConfirmAlert alert, with cancel and confirm buttons.
data AlertDialog where
  AlertDialog ::H.ToMarkup footer => { _aTitle  :: Title
                               , _aBody   :: Body
                               , _aFooter :: footer
                               } -> AlertDialog
  CancelConfirmAlert ::{ _ccaTitle  :: Title
                       , _ccaBody   :: Body
                       , _ccaConfirm :: ConfirmText
                       , _ccaConfirmAdditional :: Maybe H.Html -- ^ Additional Html for confirmation.
                       , _ccaCancel  :: CancelText
                       } -> AlertDialog

instance H.ToMarkup AlertDialog where
  toMarkup (CancelConfirmAlert title body confirmT mConfirmAdditional cancelT)
    = H.toMarkup $ AlertDialog title body footer
   where
    footer = do
      dClose
        . Helpers.classedElem H.button ["c-button", "c-button--secondary"]
        . Helpers.multiNestedClassedElems
            H.div
            ["c-button__content", "c-button__label"]
        . H.toMarkup
        $ cancelT
      dClose
        .  Helpers.classedElem H.button ["c-button", "c-button--primary"]
        .  Helpers.classedElem H.div ["c-button__content"]
        $  fromMaybe mempty mConfirmAdditional
        >> H.div (H.toMarkup confirmT)
        !  A.class_ "c-button__label"

    dClose el = el ! H.customAttribute "data-dialog-close" "data-dialog-close"

  -- An alert with a custom footer.
  toMarkup (AlertDialog title body footer) = do
    backdrop
    dialogBody
    pushDialog
   where
    backdrop =
      Helpers.classedElem H.div
                          ["c-dialog-backdrop", "c-dialog-backdrop--visible"]
                          mempty
        ! A.style "position:absolute;z-index:0;" -- this is copied from design.smart.coop; and it shouldn't really be done like this.
    dialogBody =
      let messages = do
            pushDialog
            Helpers.classedElem H.div ["c-dialog__body"]
              $ Helpers.classedElem H.div ["c-dialog__content"]
              $ do
                  Helpers.classedElem H.h3 ["c-h3"] (H.toMarkup title)
                  Helpers.classedElem H.p ["u-m-0"] (H.toMarkup body)
          dialogFooter =
            Helpers.multiNestedClassedElems
                H.div
                [ "c-dialog__footer"
                , "c-toolbar c-toolbar__spaced"
                , "c-toolbar__right"
                , "c-toolbar__item"
                , "c-button-toolbar"
                ]
              . H.toMarkup
              $ footer
      in  Helpers.classedElem H.div
                              ["c-dialog", "c-dialog--small"]
                              (messages >> dialogFooter >> pushDialog)
            ! A.role "dialog"


    pushDialog = H.div mempty ! A.class_ "c-dialog__push"

