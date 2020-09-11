module Example.Raycast (source) where

import Prelude

import Data.Color (Color)
import Data.Vec2 (Vec2)
import Data.Vec3 (Vec3(..))
import Graphics.Camera (perspectiveCam)
import Graphics.ColorRamp (grayscaleRamp)
import Graphics.DomainTransform (scale, translate)
import Graphics.SDF3 (SDF3, raymarch, sphere)
import Shader.Expr (num)
import Shader.ExprFunction (ShaderProgram, runShaderProgram, shaderProgram)
import Shader.GLSL (toGLSL)

sdf :: SDF3
sdf = translate (Vec3 {x: 0.0, y: 3.0, z: 0.0}) $ sphere 2.0

program :: ShaderProgram Vec2 Color
program = shaderProgram $ scale 500.0 $ perspectiveCam >=> raymarch sdf >=> rescaleDepth >=> grayscaleRamp
  where
    rescaleDepth d = pure $ d * num 0.2

source :: String
source = toGLSL $ runShaderProgram program
