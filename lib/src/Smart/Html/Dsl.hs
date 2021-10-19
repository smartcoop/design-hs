{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-|
Module: Smart.Html.Dsl
Description: A simple DSL for representing HTML in a more manageable way.

TODO: The fixities are fairly arbitrary at this point and should be fine tuned.

Example:

@

data Button = TodoButton

instance H.ToMarkup Button

accordion = Accordion ["Title 1" :> ("Content 1" :: Text)]
button = TodoButton

canvas :: Dsl.Canvas H.ToMarkup
canvas = accordion ::~ button ::~ EmptyCanvas

@

-}
module Smart.Html.Dsl
  ( Canvas(..)
  ) where

import qualified Text.Blaze.Html5              as H

-- | A canvas, is essentially a heterogenous list, members of which must satisfy a constraint.
-- This lets us constrain on `H.ToMarkup` etc. on the elements of this "canvas" list.
data Canvas (markup :: Type -> Constraint) where
  -- | Add a new element to the end of the canvas. 
  (:~:) ::markup a => Canvas markup -> a -> Canvas markup
  -- | Add a new element to the head of the canvas
  (::~) ::markup a => a -> Canvas markup -> Canvas markup
  -- | A canvas with a single element.
  SingletonCanvas ::markup a => a -> Canvas markup
  -- | An empty canvas
  EmptyCanvas ::Canvas markup

infixr 5 :~:
infixr 5 ::~

instance H.ToMarkup (Canvas H.ToMarkup) where
  toMarkup = \case
    -- draw the canvas first, then the element at the tail.
    canvas :~: elem'      -> H.toMarkup canvas >> H.toMarkup elem'
    -- draw the element at the head first, then the rest of the canvas.
    elem'  ::~ canvas     -> H.toMarkup elem' >> H.toMarkup canvas
    SingletonCanvas elem' -> H.toMarkup elem'
    EmptyCanvas           -> mempty
