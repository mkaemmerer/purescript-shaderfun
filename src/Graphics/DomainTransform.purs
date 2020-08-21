module Graphics.DomainTransform (translate, rotate, scale, mirrorX, mirrorY, repeatX, repeatY, repeatXY, repeatGrid) where

import Prelude hiding (mod)
import Control.Bind (composeKleisli)
import Data.Vec2 (Vec2)
import Data.VectorSpace ((^-^), (*^))
import Shader.Expr (fromVec2, projX, projY, vec2, abs, cos, sin, num, mod)
import Shader.ExprBuilder (decl)
import Shader.Function (ShaderFunc)

liftF :: forall f a b. Applicative f => (a -> b) -> a -> f b
liftF f a = pure $ f a

-- Rigidbody
translate :: forall a. Vec2 -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
translate v = composeKleisli $ liftF (_ ^-^ fromVec2 v)

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
scale fac = composeKleisli $ liftF (num (1.0 / fac) *^ _)

-- Domain repetition
mirrorX :: forall a. ShaderFunc Vec2 a -> ShaderFunc Vec2 a
mirrorX = composeKleisli $ liftF (\p -> vec2 (abs $ projX p) (projY p))

mirrorY :: forall a. ShaderFunc Vec2 a -> ShaderFunc Vec2 a
mirrorY = composeKleisli $ liftF (\p -> vec2 (projX p) (abs $ projY p))

-- export const mirror = (angle: S) =>
--   overDomain<TypeV2, TypeV2>((p) =>
--     Do(function* () {
--       const normal = yield decl(
--         vec({ x: sin(lit(-angle)), y: cos(lit(-angle)) })
--       )
--       const fac = yield decl(times(lit(2), max(lit(0), dot(normal, p))))
--       const refl = yield decl(minusV(p, timesV(fac, normal)))
--       return pure(refl)
--     })
--   )
repeatX :: forall a. Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
repeatX size = composeKleisli $ repeatX' (num size)
  where
  repeatX' cellSize p = do
    let pt = { x: projX p, y: projY p }
    halfCell <- decl $ cellSize * (num 0.5)
    p2 <- decl $ vec2 ((mod (pt.x + halfCell) cellSize) - halfCell) pt.y
    pure p2

repeatY :: forall a. Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
repeatY size = composeKleisli $ repeatY' (num size)
  where
  repeatY' cellSize p = do
    let pt = { x: projX p, y: projY p }
    halfCell <- decl $ cellSize * (num 0.5)
    p2 <- decl $ vec2 pt.x ((mod (pt.y + halfCell) cellSize) - halfCell)
    pure p2

repeatXY :: forall a. Number -> Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
repeatXY xSize ySize = repeatX xSize >>> repeatY ySize

repeatGrid :: forall a. Number -> ShaderFunc Vec2 a -> ShaderFunc Vec2 a
repeatGrid size = repeatXY size size

-- export const repeatPolar = (count: S) =>
--   overDomain<TypeV2, TypeV2>((p) =>
--     Do(function* () {
--       const angle = TAU / count
--       const halfAngle = angle * 0.5
--       const a = yield decl(plus(atan(projY(p), projX(p)), lit(halfAngle)))
--       const r = yield decl(length(p))
--       const theta = yield decl(minus(mod(a, lit(angle)), lit(halfAngle)))
--       const d = yield decl(timesV(r, vec({ x: cos(theta), y: sin(theta) })))
--       return pure(d)
--     })
--   )
-- // Domain repetition extras
-- export const repeatLogPolar = (count: S) => (sdf) => (p) =>
--   Do(function* () {
--     const r = yield decl(length(p))
--     // Apply the forward log-polar map
--     const pos = yield decl(
--       vec({
--         x: log(max(lit(0.00001), r)),
--         y: atan(projY(p), projX(p)),
--       })
--     )
--     // Scale everything so tiles will fit nicely in the [-pi,pi] interval
--     const scale = lit(count / TAU)
--     const scaled = yield decl(timesV(scale, pos))
--     const repeated = vec({
--       x: minus(mod(plus(projX(scaled), lit(0.5)), lit(1)), lit(0.5)),
--       y: minus(mod(plus(projY(scaled), lit(0.5)), lit(1)), lit(0.5)),
--     })
--     const d = yield sdf(repeated)
--     return pure(div(times(d, r), scale))
--   })
