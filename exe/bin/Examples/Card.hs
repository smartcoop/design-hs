module Examples.Card
  ( cards
  ) where

import qualified Smart.Html.Avatar             as Av
import           Smart.Html.Card

cards :: [Card]
cards =
  [ CardTitle "Card title" (Just "Some subtext")
  , CardUser $ Av.Avatar
    Av.NoAvatarImage
    Av.Small
    (Av.AvTitleAndBody "John Doe" $ Just "Backend engineer")
  , CardImage (Just "#")
              "https://via.placeholder.com/1600x900"
              "Card title"
              (Just "Subtext")
  ]
