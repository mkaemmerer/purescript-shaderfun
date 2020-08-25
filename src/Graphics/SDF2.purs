module Graphics.SDF2
  ( SDF2
  , point
  , circle
  , segment
  , box
  , polygon
  , union
  , intersection
  , difference
  , blend
  , dilate
  , outline
  , invert
  , scaleSDF
  , repeatLogPolarSDF
  ) where

import Prelude hiding (min,max)

import Control.Apply (lift2)
import Data.Array (zip, foldM, (:))
import Data.Array.NonEmpty (NonEmptyArray, fromArray, head, last, tail, toArray)
import Data.Array (length) as Array
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Data.Vec2 (Vec2)
import Data.VectorSpace ((*^), (^+^), (^-^), (<.>))
import Graphics.DomainTransform (repeatLogPolar, scale)
import Math (tau)
import Shader.Expr (Expr, abs, absV, fromVec2, gt, gte, ifE, length, lt, max, min, num, projX, projY, saturate, sqrt, vec2)
import Shader.ExprBuilder (ShaderFunc, decl)

type SDF2
  = ShaderFunc Vec2 Number

liftF :: forall f a b. Applicative f => (a -> b) -> a -> f b
liftF f a = pure $ f a

pairs :: forall a. NonEmptyArray a -> Array (Tuple a a)
pairs xs = (Tuple e s) : (zip (toArray xs) (tail xs))
  where
  s = head xs
  e = last xs

-- Geometry
point :: SDF2
point = length >>> pure

circle :: Number -> SDF2
circle r p = do
  d <- point p
  pure $ d - (num r)

projectSegment :: Expr Vec2 -> Expr Vec2 -> ShaderFunc Vec2 Vec2
projectSegment a b p = do
  pa <- decl $ p ^-^ a
  ba <- decl $ b ^-^ a
  fac <- decl $ saturate $ (pa <.> ba) / (ba <.> ba)
  c <- decl $ a ^+^ fac *^ ba
  pure c

segment :: Vec2 -> Vec2 -> SDF2
segment a b p = do
  c <- projectSegment (fromVec2 a) (fromVec2 b) p
  d <- decl $ length (p ^-^ c)
  pure d

box :: Vec2 -> SDF2
box corner p = do
  d <- decl $ (absV p) ^-^ (fromVec2 corner)
  c <- decl $ vec2 (max (projX d) zero) (max (projY d) zero)
  m <- decl $ (min (max (projX d) (projY d)) zero)
  pure $ (length c) + m

polygon :: Partial => Array Vec2 -> SDF2
polygon vs
  | Array.length vs >= 3 = polygon' $ fromJust $ fromArray vs

polygon' :: NonEmptyArray Vec2 -> SDF2
polygon' vs p = do
    pv <- decl $ p ^-^ v0
    d0 <- decl $ pv <.> pv
    s0 <- decl $ num $ if len `mod` 2 == 0 then 1.0 else -1.0
    Tuple d s <- foldM polygonSegment (Tuple d0 s0) segments
    pure $ (sqrt d) * s
  where
  v0 = fromVec2 $ head vs
  pt = { x: projX p, y: projY p }
  len = Array.length $ toArray vs
  segments = pairs $ fromVec2 <$> vs
  polygonSegment (Tuple d0 s0) (Tuple a b) = do
    -- Distance
    x <- projectSegment a b p
    c <- decl $ p ^-^ x
    d <- decl $ min d0 (c <.> c)
    -- Sign
    e <- decl $ a ^-^ b
    w <- decl $ p ^-^ b
    let et = { x: projX e, y: projY e }
    let wt = { x: projX w, y: projY w }
    c1 <- decl $ pt.y `gte` (projY b)
    c2 <- decl $ pt.y `lt` (projY a)
    c3 <- decl $ (et.x * wt.y) `gt` (et.y * wt.x)
    cc <- decl $ (c1 && c2 && c3) || (not c1 && not c2 && not c3)
    s <- decl $ s0 * (ifE cc (num $ 1.0) (num $ -1.0))
    pure $ Tuple d s

-- Transform
scaleSDF :: Number -> SDF2 -> SDF2
scaleSDF fac = (scale fac) >>> (liftA1 $ liftA1 $ adjustScale)
  where
  adjustScale d = d * num fac

repeatLogPolarSDF :: Number -> SDF2 -> SDF2
repeatLogPolarSDF count sdf p = repeatLogPolar count sdf >=> liftF adjustScale $ p
  where
  adjustScale d = d * ((length p) * (num $ tau / count))

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
