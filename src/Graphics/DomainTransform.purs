module Graphics.DomainTransform
  ( translate
  , translateX
  , translateY
  , rotate
  , scale
  , mirror
  , mirrorX
  , mirrorY
  , reflect
  , reflectX
  , reflectY
  , repeatX
  , repeatY
  , repeatXY
  , repeatGrid
  , repeatPolar
  , repeatLogPolar
  ) where

import Prelude hiding (max,mod)

import Control.Bind (composeKleisli)
import Data.Vec2 (Vec2(..))
import Data.VectorSpace ((^-^), (*^), (<.>))
import Math (tau)
import Shader.Expr (class VecExpr, abs, atan, cos, length, log, max, mod, num, projX, projY, sin, vec2)
import Shader.Expr (reflect) as S
import Shader.Expr.Cast (class Castable, cast, from)
import Shader.ExprBuilder (type (|>), decl)

liftF :: forall f a b. Applicative f => (a -> b) -> a -> f b
liftF f a = pure $ f a

overDomain :: forall dom rng. (dom |> dom) -> (dom |> rng) -> (dom |> rng)
overDomain = composeKleisli


-- | A translation by a given vector
translate :: forall a v. VecExpr v => Castable v => v -> (v |> a) -> (v |> a)
translate v = overDomain $ liftF (_ ^-^ cast v) >=> decl

-- | A 2 dimensional translation along the x axis
translateX :: forall a. Number -> (Vec2 |> a) -> (Vec2 |> a)
translateX dx = translate (Vec2 {x: dx, y: 0.0})

-- | A 2 dimensional translation along the y axis
translateY :: forall a. Number -> (Vec2 |> a) -> (Vec2 |> a)
translateY dy = translate (Vec2 {x: 0.0, y: dy})

-- | A 2 dimensional rotation around the origin, given in radians
rotate :: forall a. Number -> (Vec2 |> a) -> (Vec2 |> a)
rotate radians = overDomain $ rotate' (num (-radians))
  where
  -- TODO: add matrix type. Rewrite as matrix
  rotate' theta p = do
    let pt = from p
    cc <- decl $ cos theta
    ss <- decl $ sin theta
    p2 <- decl $ vec2 (pt.x * cc - pt.y * ss) (pt.x * ss + pt.y * cc)
    pure p2

-- | Uniform scaling by a constant factor
scale :: forall a v. VecExpr v => Number -> (v |> a) -> (v |> a)
scale fac = overDomain $ liftF (num (1.0 / fac) *^ _) >=> decl

-- | A 2d reflection across the line centered at the origin with a given polar angle
reflect :: forall a. Number -> (Vec2 |> a) -> (Vec2 |> a)
reflect radians = overDomain $ reflect' (num radians)
  where
  reflect' theta p = do
    normal <- decl $ vec2 (sin (negate theta)) (cos theta)
    p2 <- decl $ S.reflect p normal
    pure p2

-- | A 2d reflection across the X axis
reflectX :: forall a. (Vec2 |> a) -> (Vec2 |> a)
reflectX = overDomain $ liftF (\p -> vec2 (projX p) (negate $ projY p)) >=> decl

-- | A 2d reflection across the Y axis
reflectY :: forall a. (Vec2 |> a) -> (Vec2 |> a)
reflectY = overDomain $ liftF (\p -> vec2 (negate $ projX p) (projY p)) >=> decl

-- | A 2d mirroring across the line centered at the origin with a given polar angle
-- | Like `reflect` but folds the space across the axis instead, discarding the "negative" side
mirror :: forall a. Number -> (Vec2 |> a) -> (Vec2 |> a)
mirror angle = overDomain $ mirror' (num angle)
  where
  mirror' theta p = do
    normal <- decl $ vec2 (sin (negate theta)) (cos theta)
    fac <- decl $ (num 2.0) * (max zero (p <.> normal))
    p2 <- decl $ p ^-^ fac *^ normal
    pure p2

-- | A 2d mirroring across the X axis
-- | Like `reflectX` but folds the space across the axis instead, discarding the "negative" side
mirrorX :: forall a. (Vec2 |> a) -> (Vec2 |> a)
mirrorX = overDomain $ liftF (\p -> vec2 (projX p) (abs $ projY p)) >=> decl

-- | A 2d mirroring across the Y axis
-- | Like `reflectX` but folds the space across the axis instead, discarding the "negative" side
mirrorY :: forall a. (Vec2 |> a) -> (Vec2 |> a)
mirrorY = overDomain $ liftF (\p -> vec2 (abs $ projX p) (projY p)) >=> decl

-- | Repeat space along the x axis centered at the origin with a given width
repeatX :: forall a. Number -> (Vec2 |> a) -> (Vec2 |> a)
repeatX size = overDomain $ repeatX' (num size)
  where
  repeatX' cellSize p = do
    let pt = from p
    halfCell <- decl $ cellSize * (num 0.5)
    p2 <- decl $ vec2 (((pt.x + halfCell) `mod` cellSize) - halfCell) pt.y
    pure p2

-- | Repeat space along the y axis centered at the origin with a given height
repeatY :: forall a. Number -> (Vec2 |> a) -> (Vec2 |> a)
repeatY size = overDomain $ repeatY' (num size)
  where
  repeatY' cellSize p = do
    let pt = from p
    let halfCell = cellSize * num 0.5
    p2 <- decl $ vec2 pt.x (((pt.y + halfCell) `mod` cellSize) - halfCell)
    pure p2

-- | Repeat space in the x and y directions by independent factors
repeatXY :: forall a. Number -> Number -> (Vec2 |> a) -> (Vec2 |> a)
repeatXY xSize ySize = repeatX xSize >>> repeatY ySize

-- | Repeat space in the x and y directions by the same factor
repeatGrid :: forall a. Number -> (Vec2 |> a) -> (Vec2 |> a)
repeatGrid size = repeatXY size size

repeatPolar :: forall a. Number -> (Vec2 |> a) -> (Vec2 |> a)
repeatPolar count = overDomain $ repeatPolar' (num $ tau / count)
  where
  repeatPolar' angle p = do
    let pt = from p
    halfAngle <- decl $ angle * num 0.5
    r <- decl $ length p
    a <- decl $ (atan pt.y pt.x)
    t <- decl $ (a + halfAngle) `mod` angle - halfAngle
    p2 <- decl $ r *^ vec2 (cos t) (sin t)
    pure p2

repeatLogPolar :: forall a. Number -> (Vec2 |> a) -> (Vec2 |> a)
repeatLogPolar count = repeatGrid 1.0 >>> overDomain (repeatLogPolar' (num $ count / tau))
  where
  repeatLogPolar' fac p = do
    let pt = from p
    r <- decl $ length p
    p2 <- decl $ vec2 (log r) (atan pt.y pt.x)
    p3 <- decl $ fac *^ p2
    pure p3
