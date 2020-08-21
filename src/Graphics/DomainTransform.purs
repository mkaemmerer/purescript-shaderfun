module Graphics.DomainTransform
  ( translate
  , translateX
  , translateY
  , rotate
  , scale
  , mirror
  , mirrorX
  , mirrorY
  , repeatX
  , repeatY
  , repeatXY
  , repeatGrid
  , repeatPolar
  , repeatLogPolar
  ) where

import Prelude hiding (max, mod)

import Control.Bind (composeKleisli)
import Data.Vec2 (Vec2(..))
import Data.VectorSpace ((^-^), (*^), (<.>))
import Math (tau)
import Shader.Expr (abs, atan, cos, fromVec2, length, log, max, mod, num, projX, projY, sin, vec2)
import Shader.Expr (reflect) as S
import Shader.ExprBuilder (decl)
import Shader.Function (ShaderFunc)

liftF :: forall f a b. Applicative f => (a -> b) -> a -> f b
liftF f a = pure $ f a

-- Rigidbody
translate :: forall a. Vec2 -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
translate v = composeKleisli $ liftF (_ ^-^ fromVec2 v) >=> decl

translateX :: forall a. Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
translateX dx = translate (Vec2 dx 0.0)

translateY :: forall a. Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
translateY dy = translate (Vec2 0.0 dy)

rotate :: forall a. Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
rotate angle = composeKleisli $ rotate' (num (-angle))
  where
  -- TODO: add matrix type. Rewrite as matrix
  rotate' theta p = do
    let pt = { x: projX p, y: projY p }
    cc <- decl $ cos theta
    ss <- decl $ sin theta
    p2 <- decl $ vec2 (pt.x * cc - pt.y * ss) (pt.x * ss + pt.y * cc)
    pure p2

-- Not quite rigidbody
scale :: forall a. Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
scale fac = composeKleisli $ liftF (num (1.0 / fac) *^ _) >=> decl

reflect :: forall a. Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
reflect angle = composeKleisli $ reflect' (num angle)
  where
  reflect' theta p = do
    normal <- decl $ vec2 (sin (negate theta)) (cos theta)
    p2 <- decl $ S.reflect p normal
    pure p2

reflectX :: forall a. ShaderFunc Vec2 a -> ShaderFunc Vec2 a
reflectX = composeKleisli $ liftF (\p -> vec2 (negate $ projX p) (projY p)) >=> decl

reflectY :: forall a. ShaderFunc Vec2 a -> ShaderFunc Vec2 a
reflectY = composeKleisli $ liftF (\p -> vec2 (projX p) (negate $ projY p)) >=> decl

-- Domain repetition
mirror :: forall a. Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
mirror angle = composeKleisli $ mirror' (num angle)
  where
  mirror' theta p = do
    normal <- decl $ vec2 (sin (negate theta)) (cos theta)
    fac <- decl $ (num 2.0) * (max zero (p <.> normal))
    p2 <- decl $ p ^-^ fac *^ normal
    pure p2

mirrorX :: forall a. ShaderFunc Vec2 a -> ShaderFunc Vec2 a
mirrorX = composeKleisli $ liftF (\p -> vec2 (abs $ projX p) (projY p)) >=> decl

mirrorY :: forall a. ShaderFunc Vec2 a -> ShaderFunc Vec2 a
mirrorY = composeKleisli $ liftF (\p -> vec2 (projX p) (abs $ projY p)) >=> decl

repeatX :: forall a. Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
repeatX size = composeKleisli $ repeatX' (num size)
  where
  repeatX' cellSize p = do
    let pt = { x: projX p, y: projY p }
    halfCell <- decl $ cellSize * (num 0.5)
    p2 <- decl $ vec2 (((pt.x + halfCell) `mod` cellSize) - halfCell) pt.y
    pure p2

repeatY :: forall a. Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
repeatY size = composeKleisli $ repeatY' (num size)
  where
  repeatY' cellSize p = do
    let pt = { x: projX p, y: projY p }
    let halfCell = cellSize * num 0.5
    p2 <- decl $ vec2 pt.x (((pt.y + halfCell) `mod` cellSize) - halfCell)
    pure p2

repeatXY :: forall a. Number -> Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
repeatXY xSize ySize = repeatX xSize >>> repeatY ySize

repeatGrid :: forall a. Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
repeatGrid size = repeatXY size size

repeatPolar :: forall a. Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
repeatPolar count = composeKleisli $ repeatPolar' (num $ tau / count)
  where
  repeatPolar' angle p = do
    let pt = { x: projX p, y: projY p }
    halfAngle <- decl $ angle * num 0.5
    r <- decl $ length p
    a <- decl $ (atan pt.y pt.x)
    t <- decl $ (a + halfAngle) `mod` angle - halfAngle
    p2 <- decl $ r *^ vec2 (cos t) (sin t)
    pure p2

repeatLogPolar :: forall a. Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
repeatLogPolar count = repeatGrid 1.0 >>> composeKleisli (repeatLogPolar' (num $ count / tau))
  where
  repeatLogPolar' fac p = do
    let pt = { x: projX p, y: projY p }
    r <- decl $ length p
    p2 <- decl $ vec2 (log r) (atan pt.y pt.x)
    p3 <- decl $ fac *^ p2
    pure p3