module Examples.Radio
  ( radioGroups
  ) where

import           Smart.Html.Radio
import           Smart.Html.Shared.Types

radioGroups :: [RadioGroup]
radioGroups =
  [ RadioGroup "Normal radio group" buttons
  , RadioGroupInline "Inline radio group" buttons
  ]
 where
  buttons =
    uncurry RadioButton
      <$> [ (Title $ "Title " <> idx, Name $ "Name " <> idx)
          | idxI <- [0 .. 3]
          , let idx = show @Int idxI
          ]
