module Example.Compound (source) where

import Prelude

import Data.Vec2 (Vec2(..))
import Graphics.ColorRamp (signRamp)
import Graphics.DomainTransform (repeatGrid, rotate)
import Graphics.SDF2 (blend, box, circle, invert, outline)
import Shader.ExprBuilder (decl)
import Shader.Function (ShaderProgram, runShaderProgram)
import Shader.GLSL (toGLSL)

program :: ShaderProgram
program = geometry >=> decl >=> signRamp
  where
    baseGeometry = invert $ outline 10.0 $ blend (0.5) (circle 100.0) (box (Vec2 120.0 120.0))
    geometry = repeatGrid 400.0 $ rotate (0.1) baseGeometry

source :: String
source = toGLSL $ runShaderProgram program