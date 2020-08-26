module Example.LogPolar (source) where

import Prelude

import Data.Color (Color)
import Data.Vec2 (Vec2)
import Graphics.ColorRamp (signRamp)
import Graphics.SDF2 (circle, repeatLogPolarSDF, scaleSDF)
import Shader.ExprBuilder (decl)
import Shader.ExprFunction (ShaderProgram, runShaderProgram, shaderProgram)
import Shader.GLSL (toGLSL)

program :: ShaderProgram Vec2 Color
program = shaderProgram $ geometry >=> decl >=> signRamp
  where
    baseGeometry = circle 500.0
    geometry = repeatLogPolarSDF 12.0 $ scaleSDF 0.001 $ baseGeometry

source :: String
source = toGLSL $ runShaderProgram program
