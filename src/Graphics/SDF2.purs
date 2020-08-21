module Graphics.SDF2 (SDF2, point, circle, box, union, intersection, difference, blend, dilate, outline, invert, scaleSDF) where

import Prelude hiding (min,max)

import Control.Apply (lift2)
import Data.Vec2 (Vec2)
import Data.VectorSpace ((^-^))
import Graphics.DomainTransform (scale)
import Shader.Expr (abs, absV, fromVec2, length, min, max, num, projX, projY, vec2)
import Shader.ExprBuilder (decl)
import Shader.Function (ShaderFunc)

type SDF2
  = ShaderFunc Vec2 Number

-- Geometry
point :: SDF2
point = length >>> pure

circle :: Number -> SDF2
circle r p = do
  d <- point p
  pure $ d - (num r)

box :: Vec2 -> SDF2
box corner p = do
  d <- decl $ (absV p) ^-^ (fromVec2 corner)
  c <- decl $ vec2 (max (projX d) zero) (max (projY d) zero)
  m <- decl $ (min (max (projX d) (projY d)) zero)
  pure $ (length c) + m

-- Transform
scaleSDF :: Number -> SDF2 -> SDF2
scaleSDF fac = (scale fac) >>> (liftA1 $ liftA1 $ (_ * num fac))

-- Operators
union :: SDF2 -> SDF2 -> SDF2
union = lift2 $ lift2 $ min

intersection :: SDF2 -> SDF2 -> SDF2
intersection = lift2 $ lift2 $ max

difference :: SDF2 -> SDF2 -> SDF2
difference = lift2 $ lift2 $ \x y -> max x (negate y)

blend :: Number -> SDF2 -> SDF2 -> SDF2
blend fac = lift2 $ lift2 $ lerp (num fac)

-- TODO: generalize to vector spaces
lerp :: forall a. (Ring a) => a -> a -> a -> a
lerp fac x y = fac * x + (one - fac) * y

-- Morphology
dilate :: Number -> SDF2 -> SDF2
dilate fac = liftA1 $ liftA1 $ (_ - num fac)

outline :: Number -> SDF2 -> SDF2
outline fac = liftA1 $ liftA1 $ (\d -> abs d - num fac)

invert :: SDF2 -> SDF2
invert = liftA1 $ liftA1 $ negate
