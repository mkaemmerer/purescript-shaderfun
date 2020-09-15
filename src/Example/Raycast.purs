module Example.Raycast (source) where

import Prelude

import Data.Color (Color)
import Data.Vec2 (Vec2)
import Data.Vec3 (Vec3(..))
import Data.VectorSpace (dot, (*^), (^+^))
import Graphics.Camera (perspectiveCam)
import Graphics.ColorRamp (grayscaleRamp)
import Graphics.DomainTransform (scale, translate)
import Graphics.SDF3 (SDF3, raymarch, sphere, surfaceNormal)
import Shader.Expr (fst, snd, vec3)
import Shader.ExprBuilder (decl)
import Shader.ExprFunction (ShaderProgram, runShaderProgram, shaderProgram)
import Shader.GLSL (toGLSL)

sdf :: SDF3
sdf = translate (Vec3 {x: 0.0, y: 3.0, z: 0.0}) $ sphere 2.0

program :: ShaderProgram Vec2 Color
program = shaderProgram $ scale 500.0 $ perspectiveCam >=> render >=> grayscaleRamp
  where
    render ray = do
      dist   <- raymarch sdf ray
      point  <- decl $ (fst ray) ^+^ dist *^ (snd ray)
      normal <- surfaceNormal sdf point
      let light = normal `dot` (vec3 zero zero one)
      pure light

source :: String
source = toGLSL $ runShaderProgram program
