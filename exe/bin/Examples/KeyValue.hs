module Examples.KeyValue
  ( keyValueGroups
  ) where

import           Smart.Html.KeyValue
import qualified Smart.Html.Shared.Types       as Types

keyValueGroups :: [KeyValueGroup]
keyValueGroups =
  [ KeyValueGroup
      [ kvBody "Name"       "Mr. Foo"
      , kvBody "Age"        "20"
      , kvBody "Location"   "Belgium"
      , kvBody "Birth date" "12/12/2021"
      ]
  ]
  where kvBody = KeyValue @Types.Body
