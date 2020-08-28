module Graphics.Pattern (stripesHorizontal, stripesVertical, checkerboard) where

import Prelude hiding (min,max,mod)

import Data.Color (Color)
import Data.Vec2 (Vec2)
import Shader.Cast (cast)
import Shader.Expr (Expr, color, floor, num, mod, projX, projY)
import Shader.ExprBuilder (decl, type (|>))

type Pattern = Vec2 |> Color

toPoint :: Expr Vec2 -> { x :: Expr Number, y :: Expr Number }
toPoint e = { x: projX e, y: projY e }

grayscale :: Expr Number -> Expr Color
grayscale v = color v v v

xor :: forall a. Ring a => a -> a -> a
xor x y = x * (one - y) + y * (one - x)

stripesHorizontal :: Pattern
stripesHorizontal p = do
  let pt = toPoint p
  x <- decl $ (floor pt.x) `mod` (num 2.0)
  pure $ grayscale x

stripesVertical :: Pattern
stripesVertical p = do
  let pt = toPoint p
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
