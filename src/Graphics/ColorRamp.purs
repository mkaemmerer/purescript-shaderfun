module Graphics.ColorRamp (ColorRamp, signRamp, grayscaleRamp) where

import Prelude

import Data.Color (Color(..))
import Shader.Expr (Expr, color, fromColor, gt, ifE, num)
import Shader.Function (ShaderFunc)

type ColorRamp = ShaderFunc Number Color

grayscale :: Number -> Expr Color
grayscale v = fromColor $ Color v v v

white :: Expr Color
white = grayscale 1.0

black :: Expr Color
black = grayscale 0.0

signRamp :: ColorRamp
signRamp d = pure $ ifE (gt d (num 0.0)) white black

grayscaleRamp :: ColorRamp
grayscaleRamp d = pure $ color d d d