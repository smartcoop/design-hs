{- |
Module: Smart.Html.Alert
Description: Alerts

<https://design.smart.coop/development/docs/c-alert.html Docs & examples>
-}
module Smart.Html.Alert
  ( Alert(..)
  , AlertSkin(..)
  ) where

import qualified Smart.Html.Button             as Btn
import qualified Smart.Html.Dsl                as Dsl
import           Smart.Html.Dsl                 ( Canvas(..) )
import qualified Smart.Html.Shared.Html.Helpers
                                               as Helpers
import qualified Smart.Html.Shared.Html.Icons  as Icons
import           Smart.Html.Shared.Types        ( Body(..) )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | A single alert. 
data Alert where
  Alert ::KnownSymbol iconType =>
    AlertSkin -> Maybe (Icons.OSvgIconDiv iconType) -> Body -> Btn.ButtonDef -> Alert
  AlertLight ::KnownSymbol iconType =>
    AlertSkin -> Maybe (Icons.OSvgIconDiv iconType) -> Body -> Btn.ButtonDef -> Alert

data AlertSkin =
    AlertDefault
  | AlertError
  | AlertWarning
  | AlertSuccess

instance H.ToMarkup Alert where
  toMarkup = \case
    Alert skin mIcon body button -> mkAlert skin Nothing mIcon body button
    AlertLight skin mIcon body button ->
      mkAlert skin (Just "c-alert--light") mIcon body button
   where
    mkAlert skin mExtraClass mIcon body button =
      H.div
        !   A.class_ alertClass
        $   H.toMarkup
        $   Dsl.maybeEmptyCanvas @H.ToMarkup mIcon
        :~: bodyHtml
        ::~ Dsl.SingletonCanvas @H.ToMarkup button
     where
      alertClass = "c-alert " <> alertSkin <> maybe "" (" " <>) mExtraClass
      alertSkin  = case skin of
        AlertDefault -> "c-alert--default"
        AlertError   -> "c-alert--error"
        AlertWarning -> "c-alert--warning"
        AlertSuccess -> "c-alert--success"
      bodyHtml =
        Helpers.multiNestedClassedElems
            H.div
            ["c-alert__body", "c-alert__text", "c-alert__message", "c-content"]
          $ H.toMarkup body
