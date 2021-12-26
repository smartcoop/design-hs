module Examples.SideMenu
  ( sideMenus
  ) where

import           Smart.Html.Shared.Types        ( Title(..) )
import           Smart.Html.SideMenu

sideMenus :: [SideMenu]
sideMenus =
  [ SideMenuWithActive inactiveItems0 activeItem inactiveItems1 -- Active item sandwiched between inactive items
  , SideMenuWithoutActive $ inactiveItems0 <> inactiveItems1  -- No active items 
  , SideMenuWithActive mempty         activeItem inactiveItems1 -- No items before active  
  , SideMenuWithActive inactiveItems0 activeItem mempty -- No items after active  
  ]
 where
  activeItem     = SideMenuItem "This is an active item" "#active"
  inactiveItems0 = mkInactive <$> [0 .. 2]
  inactiveItems1 = mkInactive <$> [3 .. 5]
  mkInactive idx =
    SideMenuItem (Title $ "Inactive item: " <> show @Int idx) "#inactive"
