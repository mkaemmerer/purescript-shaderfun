module Graphics.SDF
  ( SDF
  , projectSegment
  , circle
  , point
  , segment
  , plane
  , union
  , intersection
  , difference
  , blend
  , dilate
  , outline
  , invert
  , scaleSDF
  ) where

import Prelude hiding (min,max)

import Control.Apply (lift2)
import Data.VectorSpace (lerp, (*^), (<.>), (^+^), (^-^))
import Shader.Expr (class VecExpr, Expr, abs, length, max, min, num, saturate)
import Shader.Expr.Cast (class Castable, cast)
import Shader.ExprBuilder (type (|>), decl)

-- | An SDF is a relation that maps a domain value to a signed distance
-- | positive distances represent the exterior, and negative distances represent the interior
type SDF domain = domain |> Number

liftF :: forall f a b. Applicative f => (a -> b) -> a -> f b
liftF f a = pure $ f a

-------------------------------------------------------------------------------
-- Geometry
-------------------------------------------------------------------------------

-- | The point closest to a query point along a line segment defined by its endpoints
projectSegment :: forall v. VecExpr v => Expr v -> Expr v -> v |> v
projectSegment a b p = do
  pa <- decl $ p ^-^ a
  ba <- decl $ b ^-^ a
  fac <- decl $ saturate $ (pa <.> ba) / (ba <.> ba)
  c <- decl $ a ^+^ fac *^ ba
  pure c

-- | `point` is a point centered at the origin
point :: forall v. VecExpr v => SDF v
point = length >>> pure

-- | `circle r` is a circle centered at the origin with radius `r`
circle :: forall v. VecExpr v => Number -> SDF v
circle r p = do
  c <- point p
  d <- decl $ c - (num r)
  pure $ d

-- | A line segment defined by its endpoints
segment :: forall v. VecExpr v => Castable v => v -> v -> SDF v
segment a b p = do
  c <- projectSegment (cast a) (cast b) p
  d <- decl $ length (p ^-^ c)
  pure d

-- | A plane centered at the origin, defined by its normal vector
plane :: forall v. VecExpr v => Castable v => v -> SDF v
plane n p = decl $ (cast n) <.> p

-------------------------------------------------------------------------------
-- Transform
-------------------------------------------------------------------------------

-- | Scale an sdf uniformly
scaleSDF :: forall v. VecExpr v => Number -> SDF v -> SDF v
scaleSDF fac sdf = scaleDomain >=> decl >=> sdf >=> scaleRange
  where
  scaleDomain v = pure $ (num (1.0 / fac)) *^ v
  scaleRange r = pure $ r * num fac

-------------------------------------------------------------------------------
-- Operators
-------------------------------------------------------------------------------

-- | Union of two SDFs
-- | Preserves the boundary, but does not yield correct distances on the interior
union :: forall d. SDF d -> SDF d -> SDF d
union = lift2 $ lift2 $ min

-- | Intersection of two SDFs
-- | Preserves the boundary, but does not yield correct distances
intersection :: forall d. SDF d -> SDF d -> SDF d
intersection = lift2 $ lift2 $ max

-- | Difference of two SDFs
-- | Preserves the boundary, but does not yield correct distances
difference :: forall d. SDF d -> SDF d -> SDF d
difference = lift2 $ lift2 $ \x y -> max x (negate y)

-- | Interpolate between two SDFs by a given factor
blend :: forall d. Number -> SDF d -> SDF d -> SDF d
blend fac = lift2 $ lift2 $ lerp (num fac)

-------------------------------------------------------------------------------
-- Morphology
-------------------------------------------------------------------------------

-- | Expand an SDF by a given factor
dilate :: forall d. Number -> SDF d -> SDF d
dilate fac = liftA1 $ liftA1 $ (_ - num fac)

-- | Expand the boundary of an SDF
outline :: forall d. Number -> SDF d -> SDF d
outline fac = liftA1 $ liftA1 $ (\d -> abs d - num fac)

-- | Reverse the sign of an SDF (swaps the interior and exterior)
invert ::forall d. SDF d -> SDF d
invert = liftA1 $ liftA1 $ negate
