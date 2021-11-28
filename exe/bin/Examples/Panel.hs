module Examples.Panel
  ( panels
  ) where

import           Smart.Html.ButtonToolbar
import           Smart.Html.Panel
import qualified Smart.Html.Shared.Html.Icons  as Icons

panels :: [Panel]
panels =
  [ PanelBody "Panel with just a body"
  , PanelHeaderAndBody "Panel Header" "Panel Body"
  , PanelHeaderBodyAndToolbar "Panel Header" "Panel Body"
    $ ButtonToolbarHorizontal [ToolbarButton Icons.svgIconAdd "Add"]
  ]
