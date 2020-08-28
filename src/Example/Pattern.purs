module Example.Pattern (source) where

import Prelude

import Data.Array ((..))
import Data.Color (Color)
import Data.Int (toNumber)
import Data.Traversable (sequence)
import Data.Vec2 (Vec2(..))
import Data.VectorSpace (averageV, (^+^), (^-^))
import Graphics.DomainTransform (rotate, scale)
import Graphics.Pattern (checkerboard)
import Math (tau)
import Shader.Expr.Cast (cast)
import Shader.ExprBuilder (ShaderFunc)
import Shader.ExprFunction (ShaderProgram, runShaderProgram, shaderProgram)
import Shader.GLSL (toGLSL)

-- TODO: would be classier to use a better reconstruction filter (tent, gaussian, etc) instead of box
oversample :: Int -> ShaderFunc Vec2 Color -> ShaderFunc Vec2 Color
oversample aa f p = liftA1 averageV $ sequence (f <$> pts)
  where
  pts = (p ^+^ _) <$> offsets
  offsets = cast <$> sampleGrid aa

sampleGrid :: Int -> Array Vec2
sampleGrid aa = vs <#> (_ ^-^ center)
  where
  count = toNumber aa
  center = Vec2 0.5 0.5
  offsets = (0 .. aa) <#> toNumber <#> (_ / count)
  vs = Vec2 <$> offsets <*> offsets

program :: ShaderProgram Vec2 Color
program = shaderProgram $ oversample 4 $ rotate (tau / 8.0) $ scale 100.0 $ checkerboard

source :: String
source = toGLSL $ runShaderProgram program
