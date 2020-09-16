module Example.Raycast (source) where

import Prelude

import Data.Color (Color)
import Data.Tuple (Tuple(..))
import Data.Vec2 (Vec2)
import Data.Vec3 (Vec3(..), unitZ)
import Data.VectorSpace (normalized, (*^), (<.>), (^+^))
import Graphics.Camera (Camera, Ray, perspectiveCam)
import Graphics.Camera as Cam
import Graphics.ColorRamp (grayscaleRamp)
import Graphics.DomainTransform (translate)
import Graphics.SDF (plane, union)
import Graphics.SDF3 (SDF3, raymarch, surfaceNormal, torus)
import Math (tau)
import Shader.Expr (num)
import Shader.Expr.Cast (cast, from)
import Shader.ExprBuilder (type (|>), decl)
import Shader.ExprFunction (ShaderProgram, runShaderProgram, shaderProgram)
import Shader.GLSL (toGLSL)

program :: ShaderProgram Vec2 Color
program = shaderProgram $ cam >=> renderLighting sdf >=> grayscaleRamp
  where
    cam :: Camera
    cam =
      Cam.zoom 1000.0 $
      Cam.rotate (tau/8.0) $
      Cam.translate (Vec3 {x: 0.0, y: -5.0, z: 1.25}) $
        perspectiveCam
    sdf :: SDF3
    sdf = union
      (translate (-1.0 *^ unitZ) $ plane unitZ)
      (torus 2.0 0.5)
      -- (sphere 1.0)
      -- (box (Vec3 {x: 1.0, y: 1.0, z: 1.0}))

renderDepth :: SDF3 -> Ray |> Number
renderDepth sdf ray = do
  dist <- raymarch sdf ray
  pure $ dist - num 2.0

renderLighting :: SDF3 -> Ray |> Number
renderLighting sdf ray = do
  let Tuple pos dir = from ray
  let lightDir = normalized $ Vec3 { x: 1.0, y: 0.0, z: 2.0 }
  dist   <- raymarch sdf ray
  point  <- decl $ pos ^+^ dist *^ dir
  normal <- surfaceNormal sdf point
  let light = normal <.> (cast lightDir)
  pure light

source :: String
source = toGLSL $ runShaderProgram program
