module Examples.Accordion
  ( accordions
  ) where

import           Smart.Html.Accordion

accordions :: [Accordion]
accordions = [Accordion ["Title 1" :> ("Content 1" :: Text)]]
