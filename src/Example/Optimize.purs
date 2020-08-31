module Example.Optimize (source) where

import Prelude

import Data.Color (Color)
import Data.Vec2 (Vec2)
import Graphics.ColorRamp (signRamp)
import Graphics.SDF (circle)
import Shader.ExprBuilder (type (|>))
import Shader.ExprFunction (ShaderProgram, runShaderProgram, shaderProgram)
import Shader.GLSL (toGLSL)

program :: ShaderProgram Vec2 Color
program = shaderProgram $ (circle 500.0) >=> change >=> signRamp

change :: Number |> Number
change e = pure ((one * one) * (e + zero))

source :: String
source = toGLSL $ runShaderProgram program
