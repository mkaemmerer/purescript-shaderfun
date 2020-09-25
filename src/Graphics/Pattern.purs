module Graphics.Pattern (stripesHorizontal, stripesVertical, checkerboard, solidColor) where

import Prelude hiding (min,max,mod)

import Data.Color (Color)
import Data.Vec2 (Vec2)
import Shader.Expr (Expr, color, floor, mod, num)
import Shader.Expr.Cast (cast, from)
import Shader.ExprBuilder (type (|>))

-- | A Pattern is a relation that maps points in the plane to colors
-- | Synonym for "Image"
type Pattern = Vec2 |> Color

grayscale :: Expr Number -> Expr Color
grayscale v = color v v v

xor :: forall a. Ring a => a -> a -> a
xor x y = x * (one - y) + y * (one - x)

-- | A repeating pattern of black and white stripes, each 1 unit tall
stripesHorizontal :: Pattern
stripesHorizontal p = pure $ grayscale y
  where
    pt = from p
    y = (floor pt.y) `mod` (num 2.0)

-- | A repeating pattern of black and white stripes, each 1 unit wide
stripesVertical :: Pattern
stripesVertical p = pure $ grayscale x
  where
    pt = from p
    x = (floor pt.x) `mod` (num 2.0)

-- | A repeating checkerboard pattern of squares each 1 unit in size
checkerboard :: Pattern
checkerboard p = do
  x <- stripesHorizontal p
  y <- stripesVertical p
  pure $ x `xor` y

-- | A pattern with uniform color
solidColor :: Color -> Pattern
solidColor c = const $ pure $ cast c
