{-# LANGUAGE ViewPatterns #-}
module Smart.Html.Brand
  ( BrandImage(..)
  , Brand(..)
  ) where

import           Smart.Html.Shared.Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

newtype BrandImage = BrandImage { _unBrandImage :: URI }
                   deriving Show
                   deriving IsString via URI

data Brand =
  Brand URI BrandImage Title
  | BrandSmall URI BrandImage Title
  | BrandXSmall URI BrandImage Title
  deriving Show

instance H.ToMarkup Brand where
  toMarkup = \case
    Brand       uri img title -> mkWithClass "c-brand--medium" uri img title
    BrandSmall  uri img title -> mkWithClass "c-brand--small" uri img title
    BrandXSmall uri img title -> mkWithClass "c-brand--xsmall" uri img title
   where
    mkWithClass (mappend "c-brand " -> class_) (URI uri) (BrandImage (URI img)) (Title title)
      = (H.div ! A.class_ class_)
        . (H.a ! A.href (H.textValue uri))
        $ H.img
        ! A.src (H.textValue img)
        ! A.alt (H.textValue title)
