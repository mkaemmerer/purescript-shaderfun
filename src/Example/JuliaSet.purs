module Example.JuliaSet (source) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)
import Data.Complex (Complex(..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Data.Vec2 (Vec2)
import Data.VectorSpace (magnitudeSquared)
import Graphics.ColorRamp (grayscaleRamp)
import Graphics.DomainTransform (scale)
import Shader.Expr (Expr, fromComplex, gt, ifE, log2, num, vec2ToComplex)
import Shader.ExprBuilder (Builder, decl)
import Shader.Function (ShaderProgram, ShaderFunc, runShaderProgram)
import Shader.GLSL (toGLSL)

type Point = Expr Complex
type Iters = Expr Number

repeatM :: forall m a. MonadRec m => Int -> (a -> m a) -> a -> m a
repeatM i action seed = tailRecM2 go i seed
  where
  go i seed
    | i <= 0    = pure $ Done seed
    | otherwise = action seed <#> (\b -> Loop { a: (i-1), b: b })

iters :: Int
iters = 200


juliaStep :: Point -> Tuple Point Iters -> Builder (Tuple Point Iters)
juliaStep c (Tuple z t) = do
  q <- decl $ (z * z) + c
  esc <- decl $ magnitudeSquared q `gt` num 4.0
  z' <- decl $ ifE esc z q
  t' <- decl $ ifE esc t (t + one)
  pure (Tuple z' t')

juliaFold :: Complex -> Point -> Builder (Tuple Point Iters)
juliaFold c z = repeatM iters (juliaStep $ fromComplex c) (Tuple z zero)

juliaSet :: Complex -> ShaderFunc Vec2 Number
juliaSet c z = do
  -- (point, iteration count)
  (Tuple z' t') <- juliaFold c (vec2ToComplex z)
  -- Smooth iteration count
  i <- decl $ t' - (log2 $ log2 $ magnitudeSquared $ z') + (num 4.0)
  f <- decl $ i / (num $ toNumber iters)
  pure f

program :: ShaderProgram
program = scale 700.0 $ fractal >=> grayscaleRamp
  where
    fractal = juliaSet $ Complex (-0.4) 0.6

source :: String
source = toGLSL $ runShaderProgram program
