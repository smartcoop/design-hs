{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Smart.Html.Grid
  ( -- * Types
    -- ** Grid
    GridElem(..)
  , Grid(..)
  -- ** Coordinates 
  , XY(..)
  , WH(..)
  -- * Type level functions
  , ElemLowerRight
  , NormaliseCoordsToOrigin
  , XYOutOfGrid
  , XYInGrid
  -- * Constraints
  , XYOutOfGridC
  , XYInGridC
  ) where

import           Data.Type.Bool
import           GHC.TypeNats
import qualified Type.Errors                   as TE

-- | @(X, Y)@ coordinates. 
data XY = XY Nat Nat

-- | @W x H@ dimensions. 
data WH = WH Nat Nat

data GridElem (xy :: XY) (hw :: WH) = GridElem

-- | A constraint that indicates whether a given XY cartesians lie within a grid. 
type family XYInGrid (gridWH :: WH) (elemXY :: XY) :: Bool where
  XYInGrid ('WH gRows gCols) ('XY ulX ulY) = ( -- on the X axis
                                                0 <=? ulX && (ulX + 1) <=? gCols -- CmpNat ulX gRows ~ 'LT
                                              -- on the Y axis
                                               &&  0 <=? ulY && (ulY + 1) <=? gRows --  CmpNat ulY gCols ~ 'LT
                                              )


-- | A constraint that indicates whether a given XY cartesians lie within a grid. 
type family XYInGridC (gridWH :: WH) (elemXY :: XY) :: Constraint where
  XYInGridC wh xy =
    If (XYInGrid wh xy) (() :: Constraint) -- An empty constraint. 
                        (TE.TypeError ('TE.ShowType xy 'TE.:<>: 'TE.Text " doesn't lie in grid " 'TE.:<>: 'TE.ShowType wh)
                        )

-- | A boolean that indicates whether a given XY cartesians lie outside a grid. 
type family XYOutOfGrid (gridWH :: WH) (elemXY :: XY) :: Bool where
  XYOutOfGrid ('WH gCols gRows) ('XY ulX ulY) = ((ulX + 1) <=? 0
                                                 || gCols <=? ulX) && ((ulY + 1) <=? 0 || gRows <=? ulY)

-- | Same as `XYOutOfGrid` but as a constraint rather than a boolean. 
type family XYOutOfGridC (gridWH :: WH) (elemXY :: XY) :: Constraint where
  XYOutOfGridC wh xy =
    If (XYOutOfGrid wh xy) (() :: Constraint)
                           (TE.TypeError ('TE.ShowType xy 'TE.:<>: 'TE.Text " lies in grid " 'TE.:<>: 'TE.ShowType wh)
                           )

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

type family OverlappingElemsFold (newElemXY :: XY) (newElemWH :: WH) (foundOverlapping :: [(XY, WH)]) (gridElems :: [(XY, WH)]) :: [(XY, WH)] where
  OverlappingElemsFold newElemXY newElemWH found ('(existingElemXY, existingElemWH) ': rest) =
    If (   XYOutOfGrid existingElemWH (NormaliseCoordsToOrigin newElemXY existingElemXY) -- Upper left of elem should not lie within existing elem
       &&  XYOutOfGrid existingElemWH (NormaliseCoordsToOrigin (ElemLowerRight newElemXY newElemWH) existingElemXY) --- Lower right shouldn't lie within existing elem
       ) (OverlappingElemsFold newElemXY newElemWH found rest) -- The new elem lies outside of this E, keep the list of found elems the same. 
         (OverlappingElemsFold newElemXY newElemWH ('(existingElemXY, existingElemWH) ': found) rest) -- The nw elem lies in this E, add it to the list of found elems. 
  OverlappingElemsFold _newElemXY _newElemWH found '[] = found

type family OverlappingElems (newElemXY :: XY) (newElemWH :: WH) (gridElems :: [(XY, WH)]) :: [(XY, WH)] where
  OverlappingElems newElemXY newElemWH existingElems = OverlappingElemsFold newElemXY newElemWH '[] existingElems

type family NewElemNotOverlappingWithExisting (newElemXY :: XY) (newElemWH :: WH) (gridElems :: [(XY, WH)]) :: Bool where
  NewElemNotOverlappingWithExisting newElemXY newElemWH ('(existingElemXY, existingElemWH) ': rest) =
       XYOutOfGrid existingElemWH (NormaliseCoordsToOrigin newElemXY existingElemXY) -- Upper left of elem should not lie within existing elem
    && XYOutOfGrid existingElemWH (NormaliseCoordsToOrigin (ElemLowerRight newElemXY newElemWH) existingElemXY) --- Lower right shouldn't lie within existing elem
    && NewElemNotOverlappingWithExisting newElemXY newElemWH rest
  NewElemNotOverlappingWithExisting _newElemXY _newElemWH '[] = 'True

type family Null (xs) :: Bool where
  Null '[] = 'True
  Null _xs = 'False 

-- | Same as `NewElemNotOverlappingWithExisting` but as a `Constraint`. 
type family NewElemNotOverlappingWithExistingC (newElemXY :: XY) (newElemWH :: WH) (existingElems :: [(XY, WH)]) :: Constraint where
  NewElemNotOverlappingWithExistingC newElemXY newElemWH existingElems =
    If (Null (OverlappingElems newElemXY newElemWH existingElems))
       ( () :: Constraint)
       ( TE.TypeError ('TE.ShowType (OverlappingElems newElemXY newElemWH existingElems)
                       'TE.:<>: 'TE.Text " overlap(s) with the new element: "
                       'TE.:<>: 'TE.ShowType newElemXY  
                       'TE.:<>: 'TE.Text " " 
                       'TE.:<>: 'TE.ShowType newElemWH  
                      )
       ) 

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

Examples of invalid grids:

@
invalidGrid :: Grid ('WH 15 15) '[ '( 'XY 10 10, 'WH 2 2) , '( 'XY 1 9 , 'WH 1 2) ]
invalidGrid = 
  GridElem @('XY 10 10) @('WH 2 2) :++ GridElem @('XY 1 9) @('WH 1 2) :++ EmptyGrid 
@

Which will result in a nicely defined compile time error message:

@
error:
    • '[ '( 'XY 1 9, 'WH 1 2)] overlap(s) with the new element: 'XY
                                                                  10 10 'WH 2 2
    • In the expression:
        GridElem @( 'XY 10 10) @( 'WH 2 2)
          :++ GridElem @( 'XY 1 9) @( 'WH 1 2) :++ EmptyGrid
      In an equation for ‘invalidGrid’:
          invalidGrid
            = GridElem @( 'XY 10 10) @( 'WH 2 2)
                :++ GridElem @( 'XY 1 9) @( 'WH 1 2) :++ EmptyGrid
@

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

infixr 5 :++

