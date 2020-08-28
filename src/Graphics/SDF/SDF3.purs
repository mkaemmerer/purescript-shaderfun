module Graphics.SDF3
  ( SDF3
  , sphere
  , box
  ) where

import Prelude hiding (min,max)

import Data.Vec3 (Vec3)
import Data.VectorSpace ((^-^))
import Graphics.SDF (circle)
import Shader.Expr (absV, length, max, min, vec3)
import Shader.Expr.Cast (cast, from)
import Shader.ExprBuilder (type (|>), decl)

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
