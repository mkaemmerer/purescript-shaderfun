module Graphics.Camera (Camera, Pos, Ray, perspectiveCam, translateCam) where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Vec2 (Vec2)
import Data.Vec3 (Vec3(..))
import Data.VectorSpace (zeroV, (^+^), (*^))
import Shader.Expr (Expr, normalized, tuple)
import Shader.Expr.Cast (cast, from)
import Shader.ExprBuilder (type (|>), decl)

type Pos = Vec3
type Ray = Tuple Pos Vec3

overPos :: Pos |> Pos -> Ray |> Ray
overPos f ray = tuple <$> f pos <*> pure dir
  where
    (Tuple pos dir) = from ray

-- | A Camera maps positions in the 2d plane to normalized rays in 3d
type Camera = Vec2 |> Ray

-- Basis vectors
up :: Expr Vec3
up  = cast $ Vec3 { x: 0.0, y: 0.0, z: 1.0 }

rt :: Expr Vec3
rt  = cast $ Vec3 { x: 1.0, y: 0.0, z: 0.0 }

fwd :: Expr Vec3
fwd = cast $ Vec3 { x: 0.0, y: 1.0, z: 0.0 }

-- | A perspective camera, centered at the origin, casting rays along the +Y direction
perspectiveCam :: Camera
perspectiveCam v = decl $ tuple ro rd
  where
    v' = from v
    rd = normalized $ fwd ^+^ v'.x *^ rt ^+^ v'.y *^ up
    ro = zeroV

-- | Translate a camera by a given vector
translateCam :: Vec3 -> Camera -> Camera
translateCam v cam = cam >=> overPos ((_ ^+^ cast v) >>> pure)
