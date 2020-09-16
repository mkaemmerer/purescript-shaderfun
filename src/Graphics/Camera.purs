module Graphics.Camera (Camera, Pos, Ray, perspectiveCam, translate, zoom, rotate) where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Vec2 (Vec2)
import Data.Vec3 (Vec3, unitX, unitY, unitZ)
import Data.VectorSpace (zeroV, (^+^), (*^))
import Graphics.DomainTransform (scale)
import Graphics.DomainTransform as Dom
import Shader.Expr (Expr, cos, normalized, sin, tuple, vec3)
import Shader.Expr.Cast (cast, from)
import Shader.ExprBuilder (type (|>), decl)

type Radians = Number
type Pos = Vec3
type Ray = Tuple Pos Vec3

overPos :: Pos |> Pos -> Ray |> Ray
overPos f ray = tuple <$> f pos <*> pure dir
  where
    (Tuple pos dir) = from ray

rotateRay :: Radians -> Ray |> Ray
rotateRay a r = do
  let Tuple pos dir = from r
  let pt = from pos
  let di = from dir
  ss <- decl $ sin $ cast a
  cc <- decl $ cos $ cast a
  pos' <- decl $ vec3 (pt.x * cc - pt.y * ss) (pt.x * ss + pt.y * cc) pt.z
  dir' <- decl $ vec3 (di.x * cc - di.y * ss) (di.x * ss + di.y * cc) di.z
  pure $ tuple pos' dir'

-- | A Camera maps positions in the 2d plane to normalized rays in 3d
type Camera = Vec2 |> Ray

-- Basis vectors
up :: Expr Vec3
up  = cast unitZ

rt :: Expr Vec3
rt  = cast unitX

fwd :: Expr Vec3
fwd = cast unitY

-- | A perspective camera, centered at the origin, casting rays along the +Y direction
perspectiveCam :: Camera
perspectiveCam v = decl $ tuple ro rd
  where
    v' = from v
    rd = normalized $ fwd ^+^ v'.x *^ rt ^+^ v'.y *^ up
    ro = zeroV

-- | Translate a camera by a given vector
translate :: Vec3 -> Camera -> Camera
translate v cam = cam >=> overPos ((_ ^+^ cast v) >>> pure)

-- | Translate a camera by a given vector
shift :: Vec2 -> Camera -> Camera
shift v = Dom.translate v

-- | Zoom in or out
zoom :: Number -> Camera -> Camera
zoom fac = scale fac
-- TODO: what units should this take? Does it need to be adjusted by a scaling factor?

-- | Rotate a camera about the origin in the XY plane.
rotate :: Radians -> Camera -> Camera
rotate a cam = cam >=> rotateRay a
-- TODO: more generalized rotations. Use a bivector?