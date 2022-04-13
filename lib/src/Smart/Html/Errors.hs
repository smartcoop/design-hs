{-# LANGUAGE OverloadedStrings #-}

module Smart.Html.Errors
  ( NotFound(..)
  ) where

import           Smart.Html.Shared.Html.Icons
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data NotFound = NotFound

instance H.ToMarkup NotFound where
  -- TODO Move the `main` element elsewhere.
  toMarkup NotFound =
      H.div ! A.class_ "c-error-page" $ do
        H.span ! A.class_ "c-error-page__code" $ "404"
        H.h4 ! A.class_ "c-error-page__title" $ "Page not found"
        H.p ! A.class_ "c-error-page__desc" $ "Sorry, we couldn't find the page you are looking for."
        H.a ! A.class_ "c-button c-button--secondary"
            ! A.href "/" $
          H.span ! A.class_ "c-button__content" $ do
            H.div ! A.class_ "o-svg-icon o-svg-icon-arrow-left  " $
              H.toMarkup svgIconArrowLeft
            H.span ! A.class_ "c-button__label" $ "Go back home"
