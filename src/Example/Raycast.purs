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
import Shader.Expr (ifE, lt, num, saturate, tuple)
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
      Cam.translate (Vec3 {x: 0.0, y: -6.0, z: 1.25}) $
        perspectiveCam
    sdf :: SDF3
    sdf = union
      (translate (-1.0 *^ unitZ) $ plane unitZ)
      (torus 2.0 1.0)
      -- (sphere 1.0)
      -- (box (Vec3 {x: 1.0, y: 1.0, z: 1.0}))

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
  -- Lighting
  let lightDir = cast $ normalized $ Vec3 { x: 1.0, y: 0.0, z: 2.0 }
  let lightRay = tuple (point ^+^ (num 0.001) *^ normal) lightDir
  let albedo   = num 0.8
  diffuse <- decl $ saturate $ normal <.> lightDir
  objDist <- raymarch sdf lightRay
  shade   <- decl $ ifE (objDist `lt` (num 10.0)) zero one
  ambient <- decl $ num 0.2
  pure $ albedo * (ambient + diffuse * shade)

source :: String
source = toGLSL $ runShaderProgram program
