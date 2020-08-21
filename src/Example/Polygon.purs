module Example.Polygon (source) where

import Prelude

import Data.Vec2 (Vec2(..))
import Graphics.ColorRamp (signRamp)
import Graphics.DomainTransform (repeatGrid, repeatPolar)
import Graphics.SDF2 (dilate, polygon)
import Math (tan, tau)
import Partial.Unsafe (unsafePartial)
import Shader.ExprBuilder (decl)
import Shader.Function (ShaderProgram, runShaderProgram)
import Shader.GLSL (toGLSL)

program :: ShaderProgram
program = geometry >=> decl >=> signRamp
  where
    count = 5.0
    a = tau / (count * 2.0)
    ss = 100.0 * tan a
    points = [
      Vec2     0.0   0.0,
      Vec2   100.0    ss,
      Vec2   200.0   0.0,
      Vec2   100.0  (-ss)
    ]
    baseGeometry = unsafePartial $ polygon points
    geometry = dilate 10.0 $ repeatGrid 800.0 $ repeatPolar count $ baseGeometry

source :: String
source = toGLSL $ runShaderProgram program