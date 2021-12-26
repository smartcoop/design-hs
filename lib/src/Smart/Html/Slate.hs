{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module:  Smart.Html.Slate 
Description: Blank slate in the Smart design showcasing system, but needn't be blank. 
-}
module Smart.Html.Slate
  ( Slate(..)
  ) where

import qualified Data.Text                     as T
import qualified Smart.Html.Dsl                as Dsl
import           Smart.Html.Dsl                 ( Canvas((::~), (:<>)) )
import           Smart.Html.Shared.Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | Slates
-- In all slates, we will only render the body-placeholder (text) when the main body canvas is empty.
-- The Placeholder canvas is always rendered, and can be used to embed actions in the slates (eg. buttons, avatars etc.)
data Slate =
  -- | Slate with a white background. 
  SlateBgWhite BodyPlaceholder (Placeholder' Dsl.HtmlCanvas) Dsl.HtmlCanvas
  -- | Slate with an alternatively shaded background. 
  | SlateBgAlt BodyPlaceholder (Placeholder' Dsl.HtmlCanvas) Dsl.HtmlCanvas

instance H.ToMarkup Slate where
  toMarkup = \case
    SlateBgWhite phText phCanvas canvas ->
      mkSlate phText phCanvas canvas Nothing
    SlateBgAlt phText phCanvas canvas ->
      mkSlate phText phCanvas canvas (Just "c-blank-slate--bg-alt")
   where
    mkSlate phText (Placeholder phCanvas) bodyCanvas mExtraClass =
      H.div ! A.class_ divClass $ body
     where
      body = case bodyCanvas of
        Dsl.EmptyCanvas -> H.toMarkup $ placeholderText ::~ phCanvas
        _               -> H.toMarkup $ phCanvas :<> bodyCanvas
      divClass = H.textValue
        (T.intercalate " " $ catMaybes [Just "c-blank-slate", mExtraClass])
      placeholderText =
        H.p ! A.class_ "u-text-muted c-body-1" $ H.toMarkup phText
