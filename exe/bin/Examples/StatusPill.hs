module Examples.StatusPill
  ( statusPills
  ) where

import           Smart.Html.StatusPill

statusPills :: [StatusPill]
statusPills =
  [ StatusPillDefault "Default"
  , StatusPillPending "Pending"
  , StatusPillSuccess "Success"
  , StatusPillDanger "Danger"
  , StatusPillWarning "Warning"
  ]
