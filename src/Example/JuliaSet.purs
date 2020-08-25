module Example.JuliaSet (source) where

import Prelude

import Data.Color (Color)
import Data.Complex (Complex(..))
import Data.Vec2 (Vec2)
import Graphics.ColorRamp (grayscaleRamp)
import Graphics.DomainTransform (scale)
import Graphics.Fractal (juliaSet)
import Shader.Expr (Expr)
import Shader.ExprFunction (ShaderProgram, runShaderProgram, shaderProgram)
import Shader.GLSL (toGLSL)

type Point = Expr Complex
type Iters = Expr Number

iters :: Int
iters = 200

program :: ShaderProgram Vec2 Color
program = shaderProgram $ scale 700.0 $ fractal >=> grayscaleRamp
  where
    fractal = juliaSet iters $ Complex (-0.4) 0.6

source :: String
source = toGLSL $ runShaderProgram program
