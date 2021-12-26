module Smart.Html.Panel
  ( Panel(..)
  ) where

import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | A Panel can have 3 forms, the simplest being with just a body.
-- Additionally, we can have headers, and headers + toolbars (toolbars can contain buttons, etc. on the RHS) 
data Panel where
  PanelBody ::Types.Body -> Panel
  PanelHeaderAndBody ::Types.Title -> Types.Body -> Panel
  PanelHeaderBodyAndToolbar ::H.ToMarkup toolbar => Types.Title -> Types.Body -> toolbar -> Panel

instance H.ToMarkup Panel where
  toMarkup = \case
    PanelBody body                -> mkPanel (Nothing @Types.Title) body
    PanelHeaderAndBody title body -> mkPanel (Just titleM) body
      where titleM = H.h2 ! A.class_ "c-panel__title" $ H.toMarkup title
    PanelHeaderBodyAndToolbar title body toolbar -> mkPanel (Just headerM) body
     where
      headerM = H.div ! A.class_ "c-toolbar" $ toolbarLeftM >> toolbarRightM
      toolbarLeftM =
        (H.div ! A.class_ "c-toolbar__left")
          . (H.div ! A.class_ "c-toolbar__item")
          . (H.h2 ! A.class_ "c-toolbar-title")
          $ H.toMarkup title
      toolbarRightM =
        (H.div ! A.class_ "c-toolbar__right")
          . (H.div ! A.class_ "c-toolbar__item")
          $ H.toMarkup toolbar

mkPanel
  :: forall header . H.ToMarkup header => Maybe header -> Types.Body -> H.Html
mkPanel mHeader body = (H.div ! A.class_ "c-panel") $ headerM >> bodyM
 where
  headerM   = maybe mempty (headerDiv . H.toMarkup) mHeader
  bodyM     = H.div ! A.class_ "c-panel__body" $ H.toMarkup body
  headerDiv = H.div ! A.class_ "c-panel__header"
