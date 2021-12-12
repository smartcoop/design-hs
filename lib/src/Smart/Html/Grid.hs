{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Smart.Html.Grid
  ( GridElem(..)
  , Grid(..)
  , XY(..)
  , WH(..)
  -- * Type level functions
  , ElemLowerRight
  , NormaliseCoordsToOrigin
  , XYOutOfGrid
  ) where

import           Data.Type.Bool
import           GHC.TypeNats

-- | @(X, Y)@ coordinates. 
data XY = XY Nat Nat

-- | @W x H@ dimensions. 
data WH = WH Nat Nat

data GridElem (xy :: XY) (hw :: WH) = GridElem

-- | A constraint that indicates whether a given XY cartesians lie within a grid. 
type family XYInGridC (gridWH :: WH) (elemXY :: XY) :: Constraint where
  XYInGridC ('WH gRows gCols) ('XY ulX ulY) = ( -- on the X axis
                                                0 <= ulX, CmpNat ulX gRows ~ 'LT
                                              -- on the Y axis
                                              , 0 <= ulY, CmpNat ulY gCols ~ 'LT
                                              )

-- | A boolean that indicates whether a given XY cartesians lie outside a grid. 
type family XYOutOfGrid (gridWH :: WH) (elemXY :: XY) :: Bool where
  XYOutOfGrid ('WH gCols gRows) ('XY ulX ulY) = ((ulX + 1) <=? 0
                                                 || gCols <=? ulX) && ((ulY + 1) <=? 0 || gRows <=? ulY)

-- | Same as `XYOutOfGrid` but as a constraint rather than a boolean. 
type family XYOutOfGridC (gridWH :: WH) (elemXY :: XY) :: Constraint where
  XYOutOfGridC wh xy = XYOutOfGrid wh xy ~ 'True

-- | Normalise coordinates to a new origin (gridXY)
-- Here the coordinate @elemXY@ is a point on the original cartesian plane, and @gridXY@ is the new origin on the cartesian plane. 
type family NormaliseCoordsToOrigin (elemXY :: XY) (newOrigin :: XY) :: XY where
  NormaliseCoordsToOrigin ('XY elemX elemY) ('XY newOriginX newOriginY) = 'XY (elemX - newOriginX) (elemY - newOriginY)

{- | A type level computation to compute the lower right corner coordinates of an element.
Examples:

@
λ> :kind! ElemLowerRight ('XY 10 10) ('WH 1 1)
ElemLowerRight ('XY 10 10) ('WH 1 1) :: XY
= 'XY 10 10
*Prelude Smart.Html.Grid
λ> :kind! ElemLowerRight ('XY 10 10) ('WH 1 2)
ElemLowerRight ('XY 10 10) ('WH 1 2) :: XY
= 'XY 10 11
@

-}
type family ElemLowerRight (elemXY :: XY) (elemWH :: WH) :: XY where
  ElemLowerRight ('XY x y) ('WH w h) =  'XY (x + (w - 1)) (y + (h - 1))

{- | Ensure that a new element doesn't overlap with existing elements of a grid. 

To evaluate this constraint, we check (iteratively) through the list of existing elements

- (we denote each existing element with E)

- (we call the new element N)

For each E:

1. Normalise N to the new origin: the Upper-left corner of E, forming a grid of width & height: WH(E) 

2. Ensure that N doesn't lie within this new "grid" of size WH(E)

That ensures that N doesn't lie within any of the @[E]@ elements already in the grid. 
-}
type family NewElemNotOverlappingWithExisting (newElemXY :: XY) (newElemWH :: WH) (gridElems :: [(XY, WH)]) :: Bool where
  NewElemNotOverlappingWithExisting newElemXY newElemWH ('(existingElemXY, existingElemWH) ': rest) =
       XYOutOfGrid existingElemWH (NormaliseCoordsToOrigin newElemXY existingElemXY) -- Upper left of elem should not lie within existing elem
    && XYOutOfGrid existingElemWH (NormaliseCoordsToOrigin (ElemLowerRight newElemXY newElemWH) existingElemXY) --- Lower right shouldn't lie within existing elem
    && NewElemNotOverlappingWithExisting newElemXY newElemWH rest
  NewElemNotOverlappingWithExisting _newElemXY _newElemWH '[] = 'True

-- | Same as `NewElemNotOverlappingWithExisting` but as a `Constraint`. 
type family NewElemNotOverlappingWithExistingC (newElemXY :: XY) (newElemWH :: WH) (existingElems :: [(XY, WH)]) :: Constraint where
  NewElemNotOverlappingWithExistingC newElemXY newElemWH existingElems =
    NewElemNotOverlappingWithExisting newElemXY newElemWH existingElems ~ 'True

{- | A Card grid, with type-level proofs that ensure elements within it, lie within its dimensions.

A grid is a coordinate plane with (0, 0) lying on the top-left corner:

@
(0, 0)
↑
.---------------------
|
|
|
|
|
|--------------------. → (w - 1, h - 1)
@

With the following properties held:

1. w > 0
2. h > 0

-}
data Grid (gWH :: WH) (elems :: [(XY, WH)]) where
  (:++) ::( XYInGridC gWH elemXY
                 , XYInGridC gWH (ElemLowerRight elemXY elemWH)
                 , NewElemNotOverlappingWithExistingC elemXY elemWH elems
                 )
               => GridElem elemXY elemWH
               -> Grid gWH elems
               -> Grid gWH ('(elemXY, elemWH) ': elems)

  EmptyGrid ::Grid gWH '[]

-- cg :: Grid ( 'WH 10 10)
-- cg = undefined
