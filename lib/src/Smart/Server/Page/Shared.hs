{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Smart.Server.Page.Shared
  ( inputField
  , spaceElem
  , spaceElemWith
  , spaceElems
  , spaceElemsWith
  , withPageHeading
  -- * Lists 
  , titledList
  ) where

-- import qualified "design-hs-lib" Smart.Html.Render
--                                                as Render
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | Generate an input field with name and type.
inputField name type' req =
  let i =
        H.input
          ! A.type_ type'
          ! A.name name
          ! A.placeholder name
          ! A.autocomplete "off"
  in  if req then i ! A.required "true" else i

-- | Space out an elem with a trailing pipe. 
spaceElem = (`spaceElemWith` H.text " | ")

-- | Space out an elem with a trailing pipe. 
spaceElemWith separator l = l >> separator

-- | Space out a list of elems with trailing pipes interspersed. 
spaceElems :: Foldable f => f H.Html -> H.Html
spaceElems = spaceElemsWith (H.text " | ")

-- | Space out a list of elems with trailing pipes interspersed. 
spaceElemsWith :: Foldable f => H.Html -> f H.Html -> H.Html
spaceElemsWith separator elems = case toList elems of
  []         -> mempty
  [h       ] -> h
  (h : rest) -> spaceElemWith separator h >> spaceElems rest

titledList
  :: forall item f
   . (H.ToMarkup item, Foldable f)
  => H.Html
  -> f item
  -> H.Html
titledList title (toList -> items) = title >> H.br >> H.ul
  (sequence_ (dispItem <$> items))
  where dispItem = H.li . H.toMarkup

-- | Add a common page heading: sets up the CSS imports, necessary encoding values etc. 
withPageHeading html =
  H.docTypeHtml
    $  H.head
    $  charset
    >> viewport
    >> maincss
    >> protocss
    >> custscss
    >> html
 where
  charset = H.meta ! A.charset "utf-8"
  viewport =
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  maincss  = stylesheet "main.css"
  protocss = stylesheet "prototype.css"
  custscss = stylesheet "styleguide-customizations.css"
  stylesheet file = H.link ! A.rel "stylesheet" ! A.href
    ("https://design.smart.coop/css/" <> file)



