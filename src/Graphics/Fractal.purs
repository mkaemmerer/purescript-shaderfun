module Graphics.Fractal (repeatM, repeatC, juliaSet, mandelbrotSet) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRec, tailRecM2)
import Data.Complex (Complex)
import Data.Either (Either)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Data.Vec2 (Vec2)
import Data.VectorSpace (magnitudeSquared)
import Shader.Expr (Expr, done, gt, ifE, log2, loop, num, tuple, vec2ToComplex)
import Shader.Expr.Cast (cast, from)
import Shader.ExprBuilder (type (|>), decl, rec)

type Point = Complex
type IterCount = Number

-- | Repeat a monadic action a specified number of times
repeatM :: forall m a. MonadRec m => Int -> (a -> m a) -> a -> m a
repeatM n action seed = tailRecM2 go n seed
  where
  go n' seed'
    | n' <= 0    = pure $ Done seed'
    | otherwise = action seed' <#> (\b -> Loop { a: (n'-1), b: b })

-- | Repeat an action a specified number of times
repeatC :: forall a f. (Category f) => Int -> f a a -> f a a
repeatC n f = tailRec go { acc: identity, count: n }
  where
  go { acc, count: 0 } = Done acc
  go { acc, count }    = Loop { acc: f >>> acc, count: count - 1}


orbitStep :: Expr Point -> (Tuple Point IterCount) |> (Either (Tuple Point IterCount) (Tuple Point IterCount))
orbitStep c zt = do
  let (Tuple z t) = from zt
  q <- decl $ (z * z) + c
  esc <- decl $ magnitudeSquared q `gt` num 4.0
  pure $ ifE esc
    (done (tuple z t))
    (loop (tuple q (t + one)))

-- | `juliaSet n c` approximates a julia set centered at `c` by iterating its orbit `n` times.
juliaSet :: Int -> Complex -> (Vec2 |> Number)
juliaSet n c z = do
  let c' = cast c
  let n' = num $ toNumber n
  -- (point, iteration count)
  (Tuple z' t') <- from <$> rec n (orbitStep c') (tuple (vec2ToComplex z) zero)
  -- Smooth iteration count
  d <- decl $ t' - (log2 $ log2 $ magnitudeSquared $ z')
  f <- decl $ d / n'
  pure f

-- | `mandelbrotSet n` approximates the mandelbrot set by iterating its orbit `n` times.
mandelbrotSet :: Int -> (Vec2 |> Number)
mandelbrotSet n c = do
  let c' = vec2ToComplex c
  let n' = num $ toNumber n
  -- (point, iteration count)
  (Tuple z' t') <- from <$> rec n (orbitStep c') (tuple zero zero)
  -- Smooth iteration count
  d <- decl $ t' - (log2 $ log2 $ magnitudeSquared $ z')
  f <- decl $ d / n'
  pure f
