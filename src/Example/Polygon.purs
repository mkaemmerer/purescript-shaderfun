module Example.Polygon (source) where

import Prelude

import Data.Color (Color)
import Data.Vec2 (Vec2(..))
import Graphics.ColorRamp (signRamp)
import Graphics.DomainTransform (repeatGrid, repeatPolar)
import Graphics.SDF (dilate)
import Graphics.SDF.SDF2 (polygon)
import Math (tan, tau)
import Partial.Unsafe (unsafePartial)
import Shader.ExprFunction (ShaderProgram, runShaderProgram, shaderProgram)
import Shader.GLSL (toGLSL)

program :: ShaderProgram Vec2 Color
program = shaderProgram $ geometry >=> signRamp
  where
    count = 5.0
    a = tau / (count * 2.0)
    ss = 100.0 * tan a
    points = [
      Vec2 { x:   0.0, y:  0.0},
      Vec2 { x: 100.0, y:   ss},
      Vec2 { x: 200.0, y:  0.0},
      Vec2 { x: 100.0, y:  -ss}
    ]
    baseGeometry = unsafePartial $ polygon points
    geometry = dilate 10.0 $ repeatGrid 800.0 $ repeatPolar count $ baseGeometry

source :: String
source = toGLSL $ runShaderProgram program
