module Graphics.ColorRamp (ColorRamp, signRamp, grayscaleRamp) where

import Prelude

import Data.Color (Color(..))
import Shader.Expr (Expr, color, gt, ifE)
import Shader.Expr.Cast (cast)
import Shader.ExprBuilder (type (|>))

-- | A Color Ramp is a relation that maps numbers to colors
type ColorRamp = Number |> Color

grayscale :: Number -> Expr Color
grayscale v = cast $ Color v v v

white :: Expr Color
white = one

black :: Expr Color
black = zero

-- | A color ramp producing black for negative values and white for positive values
-- | Useful for visualizing SDFs
signRamp :: ColorRamp
signRamp d = pure $ ifE (d `gt` zero) white black

-- | A color ramp that smoothly transitions from black to white over [0.0, 1.0]
grayscaleRamp :: ColorRamp
grayscaleRamp d = pure $ color d d d
