module Graphics.SDF.SDF2
  ( SDF2
  , box
  , polygon
  , repeatLogPolarSDF
  ) where

import Prelude hiding (min,max)

import Data.Array (length) as Array
import Data.Array (zip, foldM, (:))
import Data.Array.NonEmpty (NonEmptyArray, fromArray, head, last, tail, toArray)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Data.Vec2 (Vec2)
import Data.VectorSpace ((<.>), (^-^))
import Graphics.DomainTransform (repeatLogPolar)
import Graphics.SDF (projectSegment)
import Math (tau)
import Shader.Expr (absV, gt, gte, ifE, length, lt, max, min, num, projY, sqrt, vec2)
import Shader.Expr.Cast (cast, from)
import Shader.ExprBuilder (type (|>), decl)

-- | A signed distance field over the domain R2
type SDF2 = Vec2 |> Number

liftF :: forall f a b. Applicative f => (a -> b) -> a -> f b
liftF f a = pure $ f a

pairs :: forall a. NonEmptyArray a -> Array (Tuple a a)
pairs xs = (Tuple e s) : (zip (toArray xs) (tail xs))
  where
  s = head xs
  e = last xs

-------------------------------------------------------------------------------
-- Geometry
-------------------------------------------------------------------------------

-- | `box v` is a rectangle centered at the origin with a corner at `v`
box :: Vec2 -> SDF2
box corner p = do
  d <- decl $ (absV p) ^-^ (cast corner)
  let d' = from d
  c <- decl $ vec2 (max d'.x zero) (max d'.y zero)
  m <- decl $ (min (max d'.x d'.y) zero)
  pure $ (length c) + m

-- | A polygon, given by an array of points. Well defined on arrays with at least 3 elements.
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
  v0 = cast $ head vs
  pt = from p
  len = Array.length $ toArray vs
  segments = pairs $ cast <$> vs
  polygonSegment (Tuple d0 s0) (Tuple a b) = do
    -- Distance
    x <- projectSegment a b p
    c <- decl $ p ^-^ x
    d <- decl $ min d0 (c <.> c)
    -- Sign
    e <- decl $ a ^-^ b
    w <- decl $ p ^-^ b
    let et = from e
    let wt = from w
    c1 <- decl $ pt.y `gte` (projY b)
    c2 <- decl $ pt.y `lt` (projY a)
    c3 <- decl $ (et.x * wt.y) `gt` (et.y * wt.x)
    cc <- decl $ (c1 && c2 && c3) || (not c1 && not c2 && not c3)
    s <- decl $ s0 * (ifE cc (num $ 1.0) (num $ -1.0))
    pure $ Tuple d s

-------------------------------------------------------------------------------
-- Transform
-------------------------------------------------------------------------------

repeatLogPolarSDF :: Number -> SDF2 -> SDF2
repeatLogPolarSDF count sdf p = repeatLogPolar count sdf >=> scaleRange $ p
  where
  scaleRange d = pure $ d * ((length p) * (num $ tau / count))
