{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Smart.Html.Card
  ( Card(..)
  ) where

import           Control.Lens
import qualified Smart.Html.Avatar             as Av
import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

data Card =
  -- | A card with just a title and an optional "body" (subtext)
  CardTitle Types.Title (Maybe Types.Body)
  -- | A card with user avatar: TODO: fix html rendering, currently bugged. 
  | CardUser (Av.Avatar 'Av.WithTitleAndBody)
  -- | A card with an image. 
  | CardImage (Maybe Types.URI) Types.Image Types.Title (Maybe Types.Body)

instance H.ToMarkup Card where
  toMarkup = \case
    CardTitle title mBody -> renderUnder H.div $ titleM title >> bodyM mBody
    CardUser avatar -> renderUnder H.div $ H.toMarkup avatar
    CardImage mURI img title mBody -> case mURI of
      Nothing -> renderUnder H.div contentsM
      Just (Types.URI (H.textValue -> uri)) ->
        renderUnder (H.a ! A.href uri) contentsM
     where
      contentsM = imageM >> cardBodyM
      imageM    = H.img ! A.src (H.textValue $ img ^. coerced)
      cardBodyM =
        (H.div ! A.class_ "c-card-body") $ titleM title >> bodyM mBody
   where
    renderUnder enclosing =
      (enclosing ! A.class_ "c-card") -- Always enclosed under a root "div" or "a" 
                                      . (H.div ! A.class_ "c-card__body") -- Always enclosed under the body tag, a "div". 
    bodyM  = maybe mempty ((H.p ! A.class_ "u-text-muted") . H.toMarkup)

    titleM = (H.h4 ! A.class_ "c-card__title") . H.toMarkup
