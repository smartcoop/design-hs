module Smart.Html.ButtonToolbar
  ( ToolbarButton(..)
  , ButtonToolbar(..)
  ) where

import           Smart.Html.Shared.Html.Icons
import           Smart.Html.Shared.Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | A button in the toolbar of buttons 
data ToolbarButton = ToolbarButton Icon Title

instance H.ToMarkup ToolbarButton where
  toMarkup (ToolbarButton icon title) =
    (H.button ! A.class_ "c-button c-button--icon c-button--secondary")
      $  iconM
      >> titleM
   where
    titleM = H.div ! A.class_ "u-sr-accessible" $ H.toMarkup title
    iconM  = H.toMarkup icon

-- | A button toolbar.
-- FIXME: try with vertical non-compact?
data ButtonToolbar = ButtonToolbarHorizontal [ToolbarButton]
                   | ButtonToolbarHorizontalCompact [ToolbarButton]
                   | ButtonToolbarVerticalCompact [ToolbarButton]

instance H.ToMarkup ButtonToolbar where

  toMarkup = \case

    ButtonToolbarHorizontal buttons -> mkWithExtraClass Nothing buttons
    ButtonToolbarHorizontalCompact buttons ->
      mkWithExtraClass (Just "c-button-toolbar--compact") buttons
    ButtonToolbarVerticalCompact buttons -> mkWithExtraClass
      (Just "c-button-toolbar--compact c-button-toolbar--vertical")
      buttons
   where
    mkWithExtraClass mExtraClass buttons = (H.div ! A.class_ class_)
      $ mconcat (H.toMarkup <$> buttons)
      where class_ = "c-button-toolbar" <> maybe "" (" " <>) mExtraClass

