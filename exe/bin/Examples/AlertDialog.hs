module Examples.AlertDialog
  ( alertDialogs
  ) where

import           Smart.Html.AlertDialog
import           Smart.Html.Shared.Html.Icons   ( svgIconDelete )
import qualified Text.Blaze.Html5              as H

alertDialogs :: [AlertDialog]
alertDialogs =
  [ -- a cancel or confirm dialog.
    CancelConfirmAlert "Delete file"
                       "Are you sure?"
                       "Delete"
                       (Just $ H.toMarkup svgIconDelete)
                       "Cancel"
  ]
