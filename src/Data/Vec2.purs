module Data.Vec2 (Vec2(..)) where

import Prelude

import Data.VectorSpace (class AdditiveGroup, class InnerSpace, class VectorSpace)

data Vec2 = Vec2 { x :: Number, y :: Number }

instance additiveGroupVec2 :: AdditiveGroup Vec2 where
  zeroV = Vec2 { x: zero , y: zero }
  addV (Vec2 v1) (Vec2 v2) = Vec2 { x: v1.x + v2.x, y: v1.y + v2.y }
  subV (Vec2 v1) (Vec2 v2) = Vec2 { x: v1.x - v2.x, y: v1.y - v2.y }
  negateV (Vec2 v) = Vec2 { x: negate v.x, y: negate v.y }

instance vectorSpaceVec2 :: VectorSpace Number Vec2 where
  scale s (Vec2 v) = Vec2 { x: s * v.x, y: s * v.y }

instance innerSpaceVec2 :: InnerSpace Number Vec2 where
  dot (Vec2 v1) (Vec2 v2) = v1.x * v2.x + v1.y * v2.y
