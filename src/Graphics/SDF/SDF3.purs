module Graphics.SDF3
  ( SDF3
  , sphere
  , box
  , raymarch
  ) where

import Prelude hiding (min,max)

import Data.Tuple (Tuple(..))
import Data.Vec3 (Vec3)
import Data.VectorSpace ((^-^), (^+^), (*^))
import Graphics.SDF (circle)
import Shader.Expr (Expr, abs, done, ifE, length, loop, lte, max, min, num, projX, projY, projZ, snd, tuple, vec3)
import Shader.Expr.Cast (cast, from)
import Shader.ExprBuilder (type (|>), decl, rec)

type SDF3 = Vec3 |> Number

-- Geometry

-- | `sphere r` is a sphere centered at the origin with radius `r`
sphere :: Number -> SDF3
sphere = circle

-- | `box v` is a rectangle centered at the origin with a corner at `v`
box :: Vec3 -> SDF3
box corner p = do
  d <- decl $ (absV p) ^-^ (cast corner)
  let d' = from d
  c <- decl $ vec3 (max d'.x zero) (max d'.y zero) (max d'.z zero)
  m <- decl $ (min (max d'.x (max d'.y d'.z)) zero)
  pure $ (length c) + m

absV :: Expr Vec3 -> Expr Vec3
absV v = vec3 (abs $ projX v) (abs $ projY v) (abs $ projZ v)

type Pos = Vec3
type Ray = Tuple Pos Vec3
type Dist = Number

eps :: Expr Number
eps = num 0.0001

raymarch :: SDF3 -> Ray |> Dist
raymarch sdf ray = snd <$> rec 256 march (tuple p0 zero)
  where
  (Tuple p0 dir) = from ray
  march tup = do
    let (Tuple p t) = from tup
    p' <- decl $ p ^+^ t *^ dir
    dist <- sdf p'
    cond <- decl $ dist `lte` eps
    pure $ ifE cond
      (done (tuple p' t))
      (loop (tuple p' (t + dist)))
