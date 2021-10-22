module Examples.Accordion
  ( accordions
  ) where

import qualified Conf.Types                    as CT
import           Smart.Html.Accordion

accordions :: CT.FilesystemConf -> [Accordion]
accordions CT.FilesystemConf {..} =
  [Accordion ["Title 1" :> ("Content 1" :: Text)]]
