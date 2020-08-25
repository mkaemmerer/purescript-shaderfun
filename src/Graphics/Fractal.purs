module Graphics.Fractal (repeatM, juliaSet, mandelbrotSet) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)
import Data.Complex (Complex)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Data.Vec2 (Vec2)
import Data.VectorSpace (magnitudeSquared)
import Shader.Expr (Expr, fromComplex, gt, ifE, log2, num, vec2ToComplex)
import Shader.ExprBuilder (Builder, decl)
import Shader.Function (ShaderFunc)

type Point = Expr Complex
type IterCount = Expr Number

repeatM :: forall m a. MonadRec m => Int -> (a -> m a) -> a -> m a
repeatM i action seed = tailRecM2 go i seed
  where
  go i' seed'
    | i' <= 0    = pure $ Done seed'
    | otherwise = action seed' <#> (\b -> Loop { a: (i'-1), b: b })

orbitStep :: Point -> Tuple Point IterCount -> Builder (Tuple Point IterCount)
orbitStep c (Tuple z t) = do
  q <- decl $ (z * z) + c
  esc <- decl $ magnitudeSquared q `gt` num 4.0
  z' <- decl $ ifE esc z q
  t' <- decl $ ifE esc t (t + one)
  pure (Tuple z' t')

juliaSet :: Int -> Complex -> ShaderFunc Vec2 Number
juliaSet i c z = do
  let c' = fromComplex c
  let i' = num $ toNumber i
  -- (point, iteration count)
  (Tuple z' t') <- repeatM i (orbitStep c') (Tuple (vec2ToComplex z) zero)
  -- Smooth iteration count
  d <- decl $ t' - (log2 $ log2 $ magnitudeSquared $ z')
  f <- decl $ d / i'
  pure f

mandelbrotSet :: Int -> ShaderFunc Vec2 Number
mandelbrotSet i c = do
  let c' = vec2ToComplex c
  let i' = num $ toNumber i
  -- (point, iteration count)
  (Tuple z' t') <- repeatM i (orbitStep c') (Tuple zero zero)
  -- Smooth iteration count
  d <- decl $ t' - (log2 $ log2 $ magnitudeSquared $ z')
  f <- decl $ d / i'
  pure f