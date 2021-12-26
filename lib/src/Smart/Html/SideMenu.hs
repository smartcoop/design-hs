{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Smart.Html.SideMenu
  ( SideMenuItem(..)
  , SideMenu(..)
  , Types.ElemActiveState(..)
  ) where

import           Control.Lens
import qualified Data.Text                     as T
import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | A side-menu item. Items are parameterised by their state.
data SideMenuItem (itemActiveState :: Types.ElemActiveState) = SideMenuItem
  { _sideMenuItemTitle :: Types.Title -- ^ The title of the menu item entry 
  , _sideMenuItemURI   :: Types.URI -- ^ URI the menu item points to 
  }
  deriving Show

instance H.ToMarkup (SideMenuItem 'Types.Active) where
  toMarkup = mkSideMenuItemLi Types.Active

instance H.ToMarkup (SideMenuItem 'Types.Inactive) where
  toMarkup = mkSideMenuItemLi Types.Inactive

mkSideMenuItemLi
  :: Types.ElemActiveState -> SideMenuItem itemActiveState -> H.Html
mkSideMenuItemLi state' SideMenuItem {..} =
  (H.li ! A.class_ liClass)
    . (H.a ! A.href link' ! A.class_ "c-side-menu__link")
    $ H.toMarkup _sideMenuItemTitle
 where
  extraActiveState =
    if state' == Types.Active then Just "c-side-menu__item--active" else Nothing
  liClass = H.textValue . T.unwords $ catMaybes
    [Just "c-side-menu__item", extraActiveState]
  link' = H.textValue $ _sideMenuItemURI ^. coerced

-- | A side menu: a menu can have at most one active item, and the rest inactive. 
data SideMenu =
  SideMenuWithActive [SideMenuItem 'Types.Inactive] -- ^ Entries before the active item 
                     (SideMenuItem 'Types.Active) -- ^ The active item 
                     [SideMenuItem 'Types.Inactive] -- ^ Entries after the active item. 
  | SideMenuWithoutActive [SideMenuItem 'Types.Inactive]
  deriving Show

instance H.ToMarkup SideMenu where
  toMarkup = \case
    SideMenuWithActive preItems activeItem postItems ->
      sideMenuUl
        .  mconcat
        $  (H.toMarkup <$> preItems)
        <> [H.toMarkup activeItem]
        <> (H.toMarkup <$> postItems)
    SideMenuWithoutActive inactiveItems ->
      sideMenuUl . mconcat $ H.toMarkup <$> inactiveItems
    where sideMenuUl = H.ul ! A.class_ "c-side-menu"
