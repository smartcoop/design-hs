module Examples.Brand
  ( brands
  ) where

import           Smart.Html.Brand

brands :: [Brand]
brands =
  [ Brand "#" "https://design.smart.coop/images/logo.svg" "Smart"
  , BrandSmall "#" "https://design.smart.coop/images/logo.svg" "Smart"
  ]
