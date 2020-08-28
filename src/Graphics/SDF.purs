module Graphics.SDF
  ( SDF
  , point
  , circle
  , projectSegment
  , segment
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

type SDF domain = domain |> Number

liftF :: forall f a b. Applicative f => (a -> b) -> a -> f b
liftF f a = pure $ f a

-- Geometry
point :: forall v. VecExpr v => SDF v
point = length >>> pure

circle :: forall v. VecExpr v => Number -> SDF v
circle r p = do
  c <- point p
  d <- decl $ c - (num r)
  pure $ d

projectSegment :: forall v. VecExpr v => Expr v -> Expr v -> v |> v
projectSegment a b p = do
  pa <- decl $ p ^-^ a
  ba <- decl $ b ^-^ a
  fac <- decl $ saturate $ (pa <.> ba) / (ba <.> ba)
  c <- decl $ a ^+^ fac *^ ba
  pure c

segment :: forall v. VecExpr v => Castable v => v -> v -> SDF v
segment a b p = do
  c <- projectSegment (cast a) (cast b) p
  d <- decl $ length (p ^-^ c)
  pure d

-- Transform
scaleSDF :: forall v. VecExpr v => Number -> SDF v -> SDF v
scaleSDF fac sdf = scaleDomain >=> decl >=> sdf >=> scaleRange
  where
  scaleDomain v = pure $ (num (1.0 / fac)) *^ v
  scaleRange r = pure $ r * num fac

-- Operators
union :: forall d. SDF d -> SDF d -> SDF d
union = lift2 $ lift2 $ min

intersection :: forall d. SDF d -> SDF d -> SDF d
intersection = lift2 $ lift2 $ max

difference :: forall d. SDF d -> SDF d -> SDF d
difference = lift2 $ lift2 $ \x y -> max x (negate y)

blend :: forall d. Number -> SDF d -> SDF d -> SDF d
blend fac = lift2 $ lift2 $ lerp (num fac)

-- Morphology
dilate :: forall d. Number -> SDF d -> SDF d
dilate fac = liftA1 $ liftA1 $ (_ - num fac)

outline :: forall d. Number -> SDF d -> SDF d
outline fac = liftA1 $ liftA1 $ (\d -> abs d - num fac)

invert ::forall d. SDF d -> SDF d
invert = liftA1 $ liftA1 $ negate
