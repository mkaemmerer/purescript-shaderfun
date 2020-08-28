module Graphics.ColorRamp (ColorRamp, signRamp, grayscaleRamp) where

import Prelude

import Data.Color (Color(..))
import Shader.Expr (Expr, color, gt, ifE)
import Shader.Expr.Cast (cast)
import Shader.ExprBuilder (type (|>))

type ColorRamp = Number |> Color

grayscale :: Number -> Expr Color
grayscale v = cast $ Color v v v

white :: Expr Color
white = one

black :: Expr Color
black = zero

signRamp :: ColorRamp
signRamp d = pure $ ifE (d `gt` zero) white black

grayscaleRamp :: ColorRamp
grayscaleRamp d = pure $ color d d d
