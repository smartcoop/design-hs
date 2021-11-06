module Examples.Alert
  ( alerts
  ) where

import           Smart.Html.Alert
import           Smart.Html.Shared.Html.Icons   ( svgIconDelete )
import qualified Text.Blaze.Html5              as H

alerts :: [Alert]
alerts =
  [ -- a cancel or confirm dialog.
    CancelConfirmAlert "Delete file"
                       "Are you sure?"
                       "Delete"
                       (Just $ H.toMarkup svgIconDelete)
                       "Cancel"
  ]
