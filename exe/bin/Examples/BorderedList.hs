module Examples.BorderedList
  ( borderedLists
  ) where

import           Smart.Html.BorderedList

borderedLists :: [BorderedList]
borderedLists = verticals <> horizontals
 where
  verticals   = BorderedListVertical <$> elemGroups
  horizontals = BorderedListHorizontal <$> elemGroups
  elemGroups =
    [ BorderedListText <$> ["Apple", "Banana", "Pear"]
    , [ BorderedListText "Some text"
      , BorderedListURI "#" "Some link"
      , BorderedListURI "#" "Some other link"
      ]
    , [ BorderedListURI "#" "Some link"
      , BorderedListURI "#" "Some other link"
      , BorderedListURI "#" "Yet another link"
      ]
    ]
