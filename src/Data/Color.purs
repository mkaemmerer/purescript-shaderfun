module Data.Color (Color(..)) where

import Prelude

import Data.VectorSpace (class AdditiveGroup, class VectorSpace)

data Color
  = Color Number Number Number

instance semiringColor :: Semiring Color where
  zero = Color 0.0 0.0 0.0
  one = Color 1.0 1.0 1.0
  add (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)
  mul (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 * r2) (g1 * g2) (b1 * b2)

instance ringColor :: Ring Color where
  sub (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 - r2) (g1 - g2) (b1 - b2)

instance additiveGroupColor :: AdditiveGroup Color where
  zeroV = Color 0.0 0.0 0.0
  addV (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)
  subV (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 - r2) (g1 - g2) (b1 - b2)
  negateV (Color r g b) = Color (-r) (-g) (-b)

instance vectorSpaceColor :: VectorSpace Number Color where
  scale s (Color r g b) = Color (s*r) (s*g) (s*b)
