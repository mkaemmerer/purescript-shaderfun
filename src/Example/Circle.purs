module Example.Circle (source) where

import Prelude

import Graphics.ColorRamp (signRamp)
import Graphics.SDF2 (circle)
import Shader.ExprBuilder (decl)
import Shader.Function (ShaderProgram, runShaderProgram)
import Shader.GLSL (toGLSL)

program :: ShaderProgram
program = (circle 500.0) >=> decl >=> signRamp

source :: String
source = toGLSL $ runShaderProgram program