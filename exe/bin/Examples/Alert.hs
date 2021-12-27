{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Examples.Alert
  ( closableAlerts
  , staticAlerts
  ) where

import           Smart.Html.Alert
import qualified Smart.Html.Shared.Html.Icons  as Icons

closableAlerts :: [Alert 'AlertClosable]
closableAlerts = alerts @ 'AlertClosable

staticAlerts :: [Alert 'AlertStatic]
staticAlerts = alerts @ 'AlertStatic

iconInfo :: Icons.OSvgIconDiv "information"
iconInfo = Icons.OSvgIconDiv Icons.svgIconCircleInformation

alerts :: forall alertType . AlertTypeValue alertType => [Alert alertType]
alerts =
  [ DefaultAlert iconInfo "This is the default alert"
  , TitleContentAlert iconInfo
                      "Title of this alert"
                      ("Some rich alert body!" :: Text)
  ]

