{-# LANGUAGE TemplateHaskell #-}
module Smart.Html.Textarea
  ( Textarea(..)
  , _Textarea
  ) where

import           Control.Lens
import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | A @textarea@ input field.
data Textarea = Textarea Types.Rows Types.Id
  deriving (Eq, Show)

makePrisms ''Textarea

instance H.ToMarkup Textarea where
  toMarkup (Textarea rows id) =
    H.textarea ! A.class_ "c-textarea" ! Types.rows rows ! Types.id id $ mempty
