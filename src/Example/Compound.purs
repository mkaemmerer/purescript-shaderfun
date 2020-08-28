module Example.Compound (source) where

import Prelude

import Data.Color (Color)
import Data.Vec2 (Vec2(..))
import Graphics.ColorRamp (signRamp)
import Graphics.DomainTransform (mirror, repeatPolar, rotate, translateX)
import Graphics.SDF (blend, circle, invert, outline)
import Graphics.SDF.SDF2 (box)
import Math (tau)
import Shader.ExprFunction (ShaderProgram, runShaderProgram, shaderProgram)
import Shader.GLSL (toGLSL)

program :: ShaderProgram Vec2 Color
program = shaderProgram $ geometry >=> signRamp
  where
    baseGeometry = invert $ outline 10.0 $ blend (0.5) (circle 100.0) (box (Vec2 {x: 120.0, y: 120.0}))
    geometry = mirror (tau / 4.0) $ repeatPolar 5.0 $ translateX 400.0 $ rotate (0.1) baseGeometry

source :: String
source = toGLSL $ runShaderProgram program
