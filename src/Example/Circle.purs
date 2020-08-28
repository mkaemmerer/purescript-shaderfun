module Example.Circle (source) where

import Prelude

import Data.Color (Color)
import Data.Vec2 (Vec2)
import Graphics.ColorRamp (signRamp)
import Graphics.SDF (circle)
import Shader.ExprFunction (ShaderProgram, runShaderProgram, shaderProgram)
import Shader.GLSL (toGLSL)

program :: ShaderProgram Vec2 Color
program = shaderProgram $ (circle 500.0) >=> signRamp

source :: String
source = toGLSL $ runShaderProgram program
