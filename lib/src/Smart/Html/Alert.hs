{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{- |
Module:  Smart.Html.Alert 
Description: Alerts, not to be confused with AlertDialog and AlertStacks. 

See the <https://design.smart.coop/development/docs/c-alert.html Smart design docs>. 
-}
module Smart.Html.Alert
  ( AlertType(..)
  , Alert(..)
  , AlertTypeValue(..)
  ) where

import qualified Smart.Html.Button             as Btn
import qualified Smart.Html.Shared.Html.Icons  as Icons
import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | Sum-type to indicate if an alert is static on a page or can be closed. 
data AlertType = AlertStatic | AlertClosable
               deriving Eq

-- | A typeclass used to go from the type-level to the value level for `AlertType` kinds. 
class AlertTypeValue (at :: AlertType) where
  alertTypeValue :: AlertType

instance AlertTypeValue 'AlertStatic where
  alertTypeValue = AlertStatic

instance AlertTypeValue 'AlertClosable where
  alertTypeValue = AlertClosable

{- |

An /Alert/. Alerts can contain arbitrary HTML and can at times be closable. 

Note: We're using DataKinds here: we want to indicate at the type level if an alert can be closed using a button.
This lets us keep the number of data constructors small, and TypeSignatures can be used to indicate the kind of alert we want. 
-}
data Alert (closable :: AlertType) where
  -- | A default alert: contains just an icon and a body text. Note that this alert doesn't have a title. 
  DefaultAlert ::(AlertTypeValue closable, KnownSymbol iconType) => Icons.OSvgIconDiv iconType -> Types.Body -> Alert closable
  -- | Alerts with a distinct title and some arbitrary content. The content can be just a single paragraph of text, or some rich text
  -- as in the examples.
  TitleContentAlert ::(KnownSymbol iconType, H.ToMarkup body) => Icons.OSvgIconDiv iconType -> Types.Title -> body -> Alert closable

instance AlertTypeValue alertType => H.ToMarkup (Alert alertType) where

  toMarkup = \case
    DefaultAlert icon body ->
      mkAlert icon body Nothing (alertTypeValue @alertType)
    TitleContentAlert icon title content ->
      mkAlert icon content (Just title) (alertTypeValue @alertType)

mkAlert
  :: (KnownSymbol iconType, H.ToMarkup body)
  => Icons.OSvgIconDiv iconType
  -> body
  -> Maybe Types.Title
  -> AlertType
  -> H.Html
mkAlert icon body mTitle alertType =
  (H.div ! A.class_ "c-alert c-alert--default")
    $  H.toMarkup icon
    >> bodyM
    >> maybeCloseButton
 where
  bodyM =
    (H.div ! A.class_ "c-alert__body")
      .  (H.div ! A.class_ "c-alert__text")
      $  maybe mempty H.toMarkup mTitle
      >> bodyM'
   where
    bodyM' =
      (H.div ! A.class_ "c-alert__message")
        . (H.div ! A.class_ "c-content")
        . H.toMarkup
        $ body

  maybeCloseButton
    | alertType == AlertClosable = H.toMarkup $ Btn.ButtonIconBorderless
      (Icons.OSvgIconDiv @"close" Icons.svgIconClose)
      Nothing
    | otherwise = mempty
