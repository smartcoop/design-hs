{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Smart.Html.CardGrid
  ( GridElem(..)
  , CardGrid(..)
  , ElemLowerRight
  , XY(..)
  , WH(..)
  ) where

import           GHC.TypeNats

data XY = XY Nat Nat

data WH = WH Nat Nat

data GridElem (xy :: XY) (hw :: WH) = GridElem

-- | A constraint that indicates whether a given XY cartesians lie within a grid. 
type family XYInGridC (gridWH :: WH) (elemXY :: XY) :: Constraint where
  XYInGridC (_hwG gRows gCols) (_xyE ulX ulY) = (0 <= ulX, CmpNat ulX gRows ~ 'LT, 0 <= ulY, CmpNat ulY gCols ~ 'LT)

-- | A type level computation to compute the lower right corner coordinates of an element.
type family ElemLowerRight (elemXY :: XY) (elemWH :: WH) :: XY where
  ElemLowerRight (_xy x y) (_hw h w) =  'XY (x + (w - 1)) (y + (h - 1))

{- | A Card grid, with type-level proofs that ensure elements within it, lie within its dimensions.

A grid is a coordinate plane with (0, 0) lying on the top-left corner:

@
(0, 0)
^
|
----------------------
|
|
|
|
|
|--------------------- ( w - 1, h - 1)
@

With the following properties held:

1. w > 0
2. h > 0

-}
data CardGrid (gWH :: WH) where
  CardGridElem ::( XYInGridC gWH elemXY
                 , XYInGridC gWH (ElemLowerRight elemXY elemWH)
                 )
               => GridElem elemXY elemWH -> CardGrid gWH


-- cg :: CardGrid ( 'WH 10 10)
-- cg = undefined
