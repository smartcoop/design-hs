{-# LANGUAGE ViewPatterns #-}
module Smart.Html.Shared.Html.Helpers
  ( multiNestedClassedElems
  ) where

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
  :: Foldable f => (H.Html -> H.Html) -> f H.AttributeValue -> H.Html -> H.Html
multiNestedClassedElems mkElem (toList -> classes) inner = case classes of
  curClass : rest ->
    mkElem (multiNestedClassedElems mkElem rest inner) H.! A.class_ curClass
  [] -> inner
