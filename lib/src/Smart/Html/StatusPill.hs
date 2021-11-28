module Smart.Html.StatusPill
  ( StatusPill(..)
  ) where

import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

import qualified Smart.Html.Shared.Types       as Types

data StatusPill = StatusPillDefault Types.Title
                | StatusPillPending Types.Title
                | StatusPillSuccess Types.Title
                | StatusPillDanger Types.Title
                | StatusPillWarning Types.Title
                deriving Show

instance H.ToMarkup StatusPill where
  toMarkup = \case
    StatusPillDefault title -> mkStatusPill "default" title
    StatusPillPending title -> mkStatusPill "pending" title
    StatusPillSuccess title -> mkStatusPill "success" title
    StatusPillDanger  title -> mkStatusPill "danger" title
    StatusPillWarning title -> mkStatusPill "warning" title
   where
    mkStatusPill specificClass title =
      (H.span ! A.class_ class_) $ circleM >> titleM
     where
      circleM = H.span ! A.class_ "c-status-pill__circle" $ mempty
      titleM  = H.span ! A.class_ "c-status-pill__label" $ H.toMarkup title
      class_  = "c-status-pill c-status-pill--" <> specificClass
