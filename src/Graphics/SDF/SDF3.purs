module Graphics.SDF3
  ( SDF3
  , sphere
  , box
  , torus
  , raymarch
  , surfaceNormal
  ) where

import Prelude hiding (min,max)

import Control.Apply (lift2)
import Data.Tuple (Tuple(..))
import Data.Vec3 (Vec3)
import Data.VectorSpace ((*^), (^+^), (^-^))
import Graphics.SDF (circle)
import Shader.Expr (Expr, abs, done, ifE, length, loop, lte, max, min, normalized, num, projX, projY, projZ, vec2, vec3)
import Shader.Expr.Cast (cast, from)
import Shader.ExprBuilder (type (|>), decl, rec)

type SDF3 = Vec3 |> Number

-- Geometry

-- | `sphere r` is a sphere centered at the origin with radius `r`
sphere :: Number -> SDF3
sphere = circle

-- | `torus r1 r2` is a torus laying in the xy plane
-- | centered at the origin with major radius `r1` and minor radius `r2`
torus :: Number -> Number -> SDF3
torus r1 r2 p = do
  h <- decl $ vec2 (projX p) (projY p)
  v <- decl $ vec2 (length h - cast r1) (projZ p)
  d <- decl $ length v - (cast r2)
  pure d

-- | `box v` is a rectangle centered at the origin with a corner at `v`
box :: Vec3 -> SDF3
box corner p = do
  d <- decl $ (absV p) ^-^ (cast corner)
  let d' = from d
  c <- decl $ vec3 (max d'.x zero) (max d'.y zero) (max d'.z zero)
  m <- decl $ (min (max d'.x (max d'.y d'.z)) zero)
  r <- decl $ length c + m
  pure r

absV :: Expr Vec3 -> Expr Vec3
absV v = vec3 (abs $ projX v) (abs $ projY v) (abs $ projZ v)


-- Ray Marching

type Pos = Vec3
type Ray = Tuple Pos Vec3
type Dist = Number

eps :: Expr Number
eps = num 0.0001

raymarch :: SDF3 -> Ray |> Dist
raymarch sdf ray = rec 256 march zero
  where
  (Tuple p0 dir) = from ray
  march t = do
    p <- decl $ p0 ^+^ t *^ dir
    dist <- sdf p
    cond <- decl $ dist `lte` eps
    pure $ ifE cond
      (done (t + dist))
      (loop (t + dist))

-- Numeric surface normals via central differences
surfaceNormal :: SDF3 -> Vec3 |> Vec3
surfaceNormal sdf v = do
  let i = vec3 one zero zero
  let j = vec3 zero one zero
  let k = vec3 zero zero one
  dx <- sdf (v ^+^ eps *^ i)  `lift2 (-)`  sdf (v ^-^ eps *^ i)
  dy <- sdf (v ^+^ eps *^ j)  `lift2 (-)`  sdf (v ^-^ eps *^ j)
  dz <- sdf (v ^+^ eps *^ k)  `lift2 (-)`  sdf (v ^-^ eps *^ k)
  n  <- decl $ vec3 dx dy dz
  n' <- decl $ normalized n
  pure n'
