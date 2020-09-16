module Data.Vec3 (Vec3(..), unitX, unitY, unitZ) where

import Prelude

import Data.VectorSpace (class AdditiveGroup, class InnerSpace, class VectorSpace)

data Vec3 = Vec3 { x :: Number, y :: Number, z :: Number }

instance additiveGroupVec3 :: AdditiveGroup Vec3 where
  zeroV = Vec3 { x: zero , y: zero, z: zero }
  addV (Vec3 v1) (Vec3 v2) = Vec3 { x: v1.x + v2.x, y: v1.y + v2.y, z: v1.z + v2.z }
  subV (Vec3 v1) (Vec3 v2) = Vec3 { x: v1.x - v2.x, y: v1.y - v2.y, z: v1.z - v2.z }
  negateV (Vec3 v) = Vec3 { x: negate v.x, y: negate v.y, z: negate v.z }

instance vectorSpaceVec3 :: VectorSpace Number Vec3 where
  scale s (Vec3 v) = Vec3 { x: s * v.x, y: s * v.y, z: s * v.z }

instance innerSpaceVec3 :: InnerSpace Number Vec3 where
  dot (Vec3 v1) (Vec3 v2) = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z

unitX :: Vec3
unitX = Vec3 { x: 1.0, y: 0.0, z: 0.0 }

unitY :: Vec3
unitY = Vec3 { x: 0.0, y: 1.0, z: 0.0 }

unitZ :: Vec3
unitZ = Vec3 { x: 0.0, y: 0.0, z: 1.0 }