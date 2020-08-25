module Example.Koch (source) where

import Prelude

import Data.Color (Color)
import Data.Vec2 (Vec2(..))
import Graphics.ColorRamp (signRamp)
import Graphics.DomainTransform (mirror, mirrorX, translate)
import Graphics.Fractal (repeatC)
import Graphics.SDF2 (SDF2, dilate, scaleSDF, segment)
import Math (tau)
import Shader.ExprFunction (ShaderProgram, runShaderProgram, shaderProgram)
import Shader.GLSL (toGLSL)

kochFold :: Int -> SDF2 -> SDF2
kochFold n = (repeatC n (kFold >>> setup)) >>> inverseSetup
  where
  kFold = mirror (tau / 6.0) >>> translate (Vec2 0.5 0.0) >>> mirrorX
  setup = translate (Vec2 1.5 0.0) >>> scaleSDF (1.0 / 3.0)
  inverseSetup = scaleSDF 3.0 >>> translate (Vec2 (-1.5) 0.0)

program :: ShaderProgram Vec2 Color
program = shaderProgram $ fractal >=> signRamp
  where
    baseFractal = kochFold 8 $ segment (Vec2 0.0 0.0) (Vec2 1.0 0.0)
    fractal = dilate 2.0 $ scaleSDF 600.0 $ baseFractal

source :: String
source = toGLSL $ runShaderProgram program
