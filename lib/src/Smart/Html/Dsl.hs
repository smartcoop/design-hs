module Smart.Html.Dsl
  ( CanvasM(..)
  ) where

import qualified Text.Blaze.Html5              as H

data CanvasM a where
  (:<>) ::CanvasM a -> a -> CanvasM a
  (:><) ::a -> CanvasM a -> CanvasM a
  SingletonCanvasM ::a -> CanvasM a

instance Semigroup a => Semigroup (CanvasM a) where
  SingletonCanvasM e <> d0                 = e :>< d0
  d0                 <> SingletonCanvasM e = d0 :<> e
  d0                 <> (canvas :<> elem') = (d0 <> canvas) :<> elem'
  (canvas0 :<> elem0) <> (elem1 :>< canvas1) =
    (canvas0 :<> (elem0 <> elem1)) <> canvas1
  (elem0 :>< canvas0) <> d1 = elem0 :>< (canvas0 <> d1)

instance Monoid a => Monoid (CanvasM a) where
  mempty = SingletonCanvasM mempty

instance Functor CanvasM where
  fmap f = \case
    SingletonCanvasM a -> SingletonCanvasM $ f a
    (d0 :<> a )        -> fmap f d0 :<> f a
    (a  :>< d0)        -> f a :>< fmap f d0

-- instance Applicative CanvasM where
--   pure = SingletonCanvasM 
--   (SingletonCanvasM f) <*> d0 = fmap f d0
--   (d0 :<> f) <*> (d1 :<> elem0) = d0 <*> ((fmap f d1) :<> f elem0)

instance H.ToMarkup a => H.ToMarkup (CanvasM a) where
  toMarkup = \case
    SingletonCanvasM _ -> mempty
    canvas :<> elem'   -> H.toMarkup canvas >> H.toMarkup elem'
    elem'  :>< canvas  -> H.toMarkup elem' >> H.toMarkup canvas

-- instance Applicative CanvasM where
