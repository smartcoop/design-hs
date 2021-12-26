{- |
Module: Smart.Html.AlertStack
Description: Stack of alerts

<https://design.smart.coop/development/docs/c-alert-stack.html Docs & examples>
-}
module Smart.Html.AlertStack
  ( Btn.ButtonDef(..)
  , Alert(..)
  , AlertStack(..)
  ) where

import qualified Data.Text                     as T
import qualified Smart.Html.Button             as Btn
import qualified Smart.Html.Dsl                as Dsl
import           Smart.Html.Dsl                 ( Canvas(..) )
import qualified Smart.Html.Shared.Html.Icons  as Icons
import           Smart.Html.Shared.Types        ( Body(..) )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | A single alert. 
data Alert = Alert (Maybe Icons.Icon) Body Btn.ButtonDef
           | AlertDark (Maybe Icons.Icon) Body Btn.ButtonDef

instance H.ToMarkup Alert where
  toMarkup = \case
    Alert mIcon body button -> mkAlert Nothing mIcon body button
    AlertDark mIcon body button ->
      mkAlert (Just "ca-alert--dark") mIcon body button
   where
    mkAlert mExtraClass mIcon body button =
      H.li
        !   A.class_ alertClass
        $   H.toMarkup
        $   Dsl.maybeEmptyCanvas @H.ToMarkup mIcon
        :~: bodyHtml
        ::~ Dsl.SingletonCanvas @H.ToMarkup button
     where
      alertClass = "c-alert" <> maybe "" (" " <>) mExtraClass
      bodyHtml   = H.div ! A.class_ "c-alert__body" $ H.toMarkup body

-- | A stack of alerts. 
newtype AlertStack = AlertStack { _unAlertStack :: [Alert]  }

instance H.ToMarkup AlertStack where
  toMarkup (AlertStack alerts) =
    (H.div ! A.style mainDivStyle)
      .   (H.ul ! A.class_ "c-alert-stack" ! A.style "position: absolute")
      .   mconcat
      $   H.toMarkup
      <$> alerts
   where
    mainDivStyle =
      H.textValue
        . T.intercalate "; "
        $ [ "position: relative"
          , "width: 100%"
          , "height: 100%"
          , "min-height: 220px"
          ]

