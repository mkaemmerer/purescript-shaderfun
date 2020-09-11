module Graphics.Camera (Camera, Pos, Ray, perspectiveCam) where

import Prelude

import Data.Tuple (Tuple)
import Data.Vec2 (Vec2)
import Data.Vec3 (Vec3(..))
import Data.VectorSpace (zeroV, (^+^), (*^))
import Shader.Expr (tuple)
import Shader.Expr.Cast (cast, from)
import Shader.Expr.Normalization (normalize)
import Shader.ExprBuilder (type (|>), decl)

type Pos = Vec3
type Ray = Tuple Pos Vec3

-- | A Camera maps positions in the 2d plane to normalized rays in 3d
type Camera = Vec2 |> Ray

-- Basis vectors
up :: Vec3
up  = Vec3 { x: 0.0, y: 0.0, z: 1.0 }

rt :: Vec3
rt  = Vec3 { x: 1.0, y: 0.0, z: 0.0 }

fwd :: Vec3
fwd = Vec3 { x: 0.0, y: 1.0, z: 0.0 }

-- | A perspective camera, centered at the origin, casting rays along the +Y direction
perspectiveCam :: Camera
perspectiveCam v = decl $ tuple ro rd
  where
    v' = from v
    rd = normalize $ (cast fwd) ^+^ v'.x *^ (cast rt) ^+^ v'.y *^ (cast up)
    ro = zeroV