module Example.LogPolar (source) where

import Prelude

import Graphics.ColorRamp (signRamp)
import Graphics.SDF2 (circle, repeatLogPolarSDF, scaleSDF)
import Shader.ExprBuilder (decl)
import Shader.Function (ShaderProgram, runShaderProgram)
import Shader.GLSL (toGLSL)

program :: ShaderProgram
program = geometry >=> decl >=> signRamp
  where
    baseGeometry = circle 500.0
    geometry = repeatLogPolarSDF 12.0 $ scaleSDF 0.001 $ baseGeometry

source :: String
source = toGLSL $ runShaderProgram program