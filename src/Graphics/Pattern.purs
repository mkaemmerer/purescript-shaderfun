module Graphics.Pattern (stripesHorizontal, stripesVertical, checkerboard) where

import Prelude hiding (min,max,mod)

import Data.Color (Color)
import Data.Vec2 (Vec2)
import Shader.Expr (Expr, color, floor, mod, num)
import Shader.Expr.Cast (cast, from)
import Shader.ExprBuilder (decl, type (|>))

type Pattern = Vec2 |> Color

grayscale :: Expr Number -> Expr Color
grayscale v = color v v v

xor :: forall a. Ring a => a -> a -> a
xor x y = x * (one - y) + y * (one - x)

stripesHorizontal :: Pattern
stripesHorizontal p = do
  let pt = from p
  x <- decl $ (floor pt.x) `mod` (num 2.0)
  pure $ grayscale x

stripesVertical :: Pattern
stripesVertical p = do
  let pt = from p
  y <- decl $ (floor pt.y) `mod` (num 2.0)
  pure $ grayscale y

checkerboard :: Pattern
checkerboard p = do
  x <- stripesHorizontal p
  y <- stripesVertical p
  c <- decl $ x `xor` y
  pure c

solidColor :: Color -> Pattern
solidColor c = const $ pure $ cast c
