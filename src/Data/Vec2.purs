module Data.Vec2 (Vec2(..)) where

import Prelude

import Data.VectorSpace (class AdditiveGroup, class InnerSpace, class VectorSpace)

data Vec2
  = Vec2 Number Number

instance additiveGroupVec2 :: AdditiveGroup Vec2 where
  zeroV = Vec2 0.0 0.0
  addV (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)
  subV (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)
  negateV (Vec2 x y) = Vec2 (-x) (-y)

instance vectorSpaceVec2 :: VectorSpace Number Vec2 where
  scale s (Vec2 x y) = Vec2 (s*x) (s*y)

instance innerSpaceVec2 :: InnerSpace Number Vec2 where
  dot (Vec2 x1 y1) (Vec2 x2 y2) = x1 * x2 + y1 * y2
