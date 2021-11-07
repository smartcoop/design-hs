module Examples.Slate
  ( slates
  ) where

import           Smart.Html.Button
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Shared.Html.Icons  as Icons
import           Smart.Html.Shared.Types        ( Placeholder'(..) )
import           Smart.Html.Slate

slates :: [Slate]
slates =
  whiteAndAlt "There are no items yet."
              (Placeholder Dsl.EmptyCanvas)
              Dsl.EmptyCanvas
    <> whiteAndAlt
         "There are no items yet."
         (Placeholder . Dsl.SingletonCanvas $ ButtonSecondaryIconAndText
           Icons.svgIconAdd
           "Add item"
         )
         Dsl.EmptyCanvas

 where
  whiteAndAlt bodyPlaceholder placeholderCanvas canvas =
    [ SlateBgWhite bodyPlaceholder placeholderCanvas canvas
    , SlateBgAlt bodyPlaceholder placeholderCanvas canvas
    ]
