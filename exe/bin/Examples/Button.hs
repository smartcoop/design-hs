{-# LANGUAGE OverloadedStrings #-}
module Examples.Button
  ( buttons
  , buttonNames
  , buttonCanvases
  ) where

import           Smart.Html.Button
import qualified Smart.Html.Dsl                as Dsl
import           Smart.Html.Dsl                 ( Canvas((::~)) ) -- import operators for neatness.
import qualified Smart.Html.Shared.Html.Icons  as Icons
import           Smart.Html.Shared.Types        ( ElemEnabledState(..) )
import qualified Text.Blaze.Html5              as H

buttons :: [Button]
buttons = snd <$> buttonNames

buttonNames :: [(Text, Button)]
buttonNames =
  disabledAndEnabled ("Button primary", ButtonPrimary "Button primary")
    <> disabledAndEnabled
         ("Button secondary", ButtonSecondary "Button secondary")
    <> [ ( "Button danger secondary"
         , ButtonDangerSecondary "Button danger secondary"
         )
       , ("Button danger", ButtonDanger "Button danger")
       , ( "Button secondary icon + text"
         , ButtonSecondaryIconAndText Icons.svgIconEdit
                                      "Button secondary icon + text"
         )
       , ( "Button secondary text + icon"
         , ButtonSecondaryTextAndIcon "Button secondary text + icon"
                                      Icons.svgIconChevronRight
         )
       , ("Button borderless"       , ButtonBorderless "Button borderless")
       , ("Button with just an icon", ButtonIcon Icons.svgIconAdd)
       ]
 where
  disabledAndEnabled (bName, cons') =
    [ (bName, cons' en) | en <- [Enabled, Disabled] ]

buttonCanvases :: [Dsl.HtmlCanvas]
buttonCanvases = uncurry mkButtonCanvas <$> buttonNames

-- | Create a button canvas. 
mkButtonCanvas :: Text -> Button -> Dsl.HtmlCanvas
mkButtonCanvas bName b = bName ::~ H.br ::~ b ::~ Dsl.SingletonCanvas H.br
