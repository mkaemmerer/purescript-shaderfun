module Example.Raycast (source) where

import Prelude

import Data.Color (Color)
import Data.Tuple (Tuple(..))
import Data.Vec2 (Vec2)
import Data.Vec3 (Vec3(..))
import Data.VectorSpace (dot, (*^), (^+^))
import Graphics.Camera (Camera, Ray, perspectiveCam, translateCam)
import Graphics.ColorRamp (grayscaleRamp)
import Graphics.DomainTransform (scale, translate)
import Graphics.SDF3 (SDF3, raymarch, surfaceNormal, torus)
import Shader.Expr (num, vec3)
import Shader.Expr.Cast (from)
import Shader.ExprBuilder (type (|>), decl)
import Shader.ExprFunction (ShaderProgram, runShaderProgram, shaderProgram)
import Shader.GLSL (toGLSL)

program :: ShaderProgram Vec2 Color
program = shaderProgram $ scale 500.0 $ cam >=> renderLighting sdf >=> grayscaleRamp
  where
    cam :: Camera
    cam = translateCam (Vec3 {x: 0.0, y: -3.0, z: 0.75}) $ perspectiveCam
    sdf :: SDF3
    sdf = translate (Vec3 {x: 0.0, y: 0.0, z: 0.0}) $
      torus 2.0 0.25
      -- sphere 2.0
      -- box (Vec3 {x: 1.0, y: 1.0, z: 1.0})

renderDepth :: SDF3 -> Ray |> Number
renderDepth sdf ray = do
  dist <- raymarch sdf ray
  pure $ dist - num 2.0

renderLighting :: SDF3 -> Ray |> Number
renderLighting sdf ray = do
  let Tuple pos dir = from ray
  dist   <- raymarch sdf ray
  point  <- decl $ pos ^+^ dist *^ dir
  normal <- surfaceNormal sdf point
  let light = normal `dot` (vec3 zero zero one)
  pure light

source :: String
source = toGLSL $ runShaderProgram program
