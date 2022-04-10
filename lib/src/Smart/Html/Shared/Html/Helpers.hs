{-# LANGUAGE ViewPatterns #-}
module Smart.Html.Shared.Html.Helpers
  ( multiNestedClassedElems
  , classedElem
  , unorderedList
  ) where

import qualified Data.Text                     as T
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

{- | Combinator to use multiple classes and nest divs/elems under these classes without a mess.

Consider:

@
<div class="foo">
  <div class="bar">
    <div class="far">
      <p>Hello, world!</p>
... 
@

This can be achieved with:

@
multiNestedClassedElems H.div ["foo", "bar", "far"] $ H.p "Hello, world!"
@

Which is a lot easier on the eyes.
-}
multiNestedClassedElems
  :: Foldable f => (H.Html -> H.Html) -> f Text -> H.Html -> H.Html
multiNestedClassedElems mkElem (toList -> classes) inner = case classes of
  curClass : rest -> classedElem mkElem
                                 (Identity curClass)
                                 (multiNestedClassedElems mkElem rest inner)
  [] -> inner

-- | Create an element with a class
classedElem :: Foldable f => (H.Html -> H.Html) -> f Text -> H.Html -> H.Html
classedElem mkElem classes inner = mkElem inner H.! A.class_ classesConcat
  where classesConcat = H.textValue $ T.intercalate " " (toList classes)

unorderedList :: Foldable f => f H.Html -> H.Html
unorderedList = H.ul . mapM_ H.li . toList
