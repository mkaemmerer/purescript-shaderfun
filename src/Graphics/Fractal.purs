module Graphics.Fractal (repeatM, repeatC, juliaSet, mandelbrotSet) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRec, tailRecM2)
import Data.Complex (Complex)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Data.Vec2 (Vec2)
import Data.VectorSpace (magnitudeSquared)
import Shader.Expr (Expr, fromComplex, gt, ifE, log2, num, vec2ToComplex)
import Shader.ExprBuilder (ShaderFunc, Builder, decl)

type Point = Expr Complex
type IterCount = Expr Number

repeatM :: forall m a. MonadRec m => Int -> (a -> m a) -> a -> m a
repeatM n action seed = tailRecM2 go n seed
  where
  go n' seed'
    | n' <= 0    = pure $ Done seed'
    | otherwise = action seed' <#> (\b -> Loop { a: (n'-1), b: b })

repeatC :: forall a f. (Category f) => Int -> f a a -> f a a
repeatC n f = tailRec go { acc: identity, count: n }
  where
  go { acc, count: 0 } = Done acc
  go { acc, count }    = Loop { acc: f >>> acc, count: count - 1}

orbitStep :: Point -> Tuple Point IterCount -> Builder (Tuple Point IterCount)
orbitStep c (Tuple z t) = do
  q <- decl $ (z * z) + c
  esc <- decl $ magnitudeSquared q `gt` num 4.0
  z' <- decl $ ifE esc z q
  t' <- decl $ ifE esc t (t + one)
  pure (Tuple z' t')

juliaSet :: Int -> Complex -> ShaderFunc Vec2 Number
juliaSet n c z = do
  let c' = fromComplex c
  let n' = num $ toNumber n
  -- (point, iteration count)
  (Tuple z' t') <- repeatM n (orbitStep c') (Tuple (vec2ToComplex z) zero)
  -- Smooth iteration count
  d <- decl $ t' - (log2 $ log2 $ magnitudeSquared $ z')
  f <- decl $ d / n'
  pure f

mandelbrotSet :: Int -> ShaderFunc Vec2 Number
mandelbrotSet n c = do
  let c' = vec2ToComplex c
  let n' = num $ toNumber n
  -- (point, iteration count)
  (Tuple z' t') <- repeatM n (orbitStep c') (Tuple zero zero)
  -- Smooth iteration count
  d <- decl $ t' - (log2 $ log2 $ magnitudeSquared $ z')
  f <- decl $ d / n'
  pure f
