module Data.Color (Color(..), class ColorSpace, timesC, (**)) where

import Prelude

import Data.VectorSpace (class AdditiveGroup, class VectorSpace)

data Color
  = Color Number Number Number

instance additiveGroupColor :: AdditiveGroup Color where
  zeroV = Color 0.0 0.0 0.0
  addV (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)
  subV (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 - r2) (g1 - g2) (b1 - b2)
  negateV (Color r g b) = Color (-r) (-g) (-b)

instance vectorSpaceColor :: VectorSpace Number Color where
  scale s (Color r g b) = Color (s*r) (s*g) (s*b)


class (VectorSpace scalar color) <= ColorSpace scalar color | color -> scalar where
  timesC :: color -> color -> color

instance colorSpaceColor :: ColorSpace Number Color where
  timesC (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1*r2) (g1*g2) (b1*b2)

infixr 7 timesC as **
