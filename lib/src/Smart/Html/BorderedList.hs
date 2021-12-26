{-# LANGUAGE ViewPatterns #-}
{- |
Module: Smart.Html.BorderedList
Description: Bordered lists (elems are separated by borders)

<https://design.smart.coop/development/docs/c-bordered-list.html Docs & examples>
-}
module Smart.Html.BorderedList
  ( BorderedList(..)
  , BorderedListElem(..)
  ) where

import           Smart.Html.Shared.Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

data BorderedListElem = BorderedListText Title
                      | BorderedListURI URI Title
                      deriving Show

instance H.ToMarkup BorderedListElem where
  toMarkup = \case
    BorderedListText title -> H.li . H.span $ H.toMarkup title
    BorderedListURI (URI (H.textValue -> uri)) title ->
      H.li . (H.a ! A.href uri) $ H.toMarkup title

data BorderedList =
  BorderedListVertical [BorderedListElem]
  | BorderedListHorizontal [BorderedListElem]
  deriving Show

instance H.ToMarkup BorderedList where
  toMarkup = \case
    BorderedListVertical elems -> mkWithClass "c-bordered-list" elems
    BorderedListHorizontal elems ->
      mkWithClass "c-bordered-list-horizontal" elems
   where
    mkWithClass class_ elems =
      (H.ul ! A.class_ class_) . mconcat $ H.toMarkup <$> elems
