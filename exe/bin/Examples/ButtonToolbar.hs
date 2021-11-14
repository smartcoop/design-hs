module Examples.ButtonToolbar
  ( buttonToolbars
  ) where

import           Smart.Html.ButtonToolbar
import qualified Smart.Html.Shared.Html.Icons  as Icons

buttonToolbars :: [ButtonToolbar]
buttonToolbars =
  [ ButtonToolbarHorizontal [addB, delB]
  , ButtonToolbarHorizontalCompact [addB, editB, delB]
  , ButtonToolbarVerticalCompact [addB, editB, delB]
  ]

 where
  addB  = ToolbarButton Icons.svgIconAdd "Add"
  editB = ToolbarButton Icons.svgIconEdit "Edit"
  delB  = ToolbarButton Icons.svgIconDelete "Delete"
