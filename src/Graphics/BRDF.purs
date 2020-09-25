module Graphics.BRDF (diffuseBRDF, emissiveBRDF, specularBRDF, toBRDFInput, sample, BRDF, BRDFInput, BRDFInputRecord) where

import Prelude

import Control.Apply (lift2)
import Data.Color (Color)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Data.Vec3 (Vec3)
import Data.VectorSpace (class AdditiveGroup, class VectorSpace, (*^), (<.>), (^+^))
import Shader.Expr (Expr, normalized, pow, saturate, tuple)
import Shader.Expr.Cast (cast, from)
import Shader.ExprBuilder (type (|>), Builder)

-- TODO: Why can't I use a newtype wrapper instead?
data BRDF = BRDF (BRDFInput |> Color)

-- Would it be better/possible to curry?
type BRDFInput = Vec3 /\ Vec3 /\ Vec3 /\ Color
type BRDFInputRecord = {
  view :: Expr Vec3,
  normal :: Expr Vec3,
  light :: {
    direction :: Expr Vec3,
    intensity :: Expr Color -- TODO: factor this out?
  }
}

wrap :: (BRDFInput |> Color) -> BRDF
wrap = BRDF

unwrap :: BRDF -> (BRDFInput |> Color)
unwrap (BRDF f) = f

over ::
  (BRDFInput |> Color -> BRDFInput |> Color) ->
  (BRDF -> BRDF)
over f x = wrap $ f (unwrap x)

over2 ::
  (BRDFInput |> Color -> BRDFInput |> Color -> BRDFInput |> Color) ->
  (BRDF -> BRDF -> BRDF)
over2 f x y = wrap $ f (unwrap x) (unwrap y)

instance semiringBRDF :: Semiring BRDF where
  zero = wrap $ const $ pure zero
  one  = wrap $ const $ pure one
  add  = over2 $ lift2 $ lift2 (+)
  mul  = over2 $ lift2 $ lift2 (*)

instance ringBRDF :: Ring BRDF where
  sub  = over2 $ lift2 $ lift2 (-)

instance additiveBRDF :: AdditiveGroup BRDF where
  zeroV   = zero
  addV    = add
  subV    = sub
  negateV = negate

instance vectorSpaceBRDF :: VectorSpace (Expr Number) BRDF where
  scale x = over $ map $ map ((*^) x)


toBRDFInput :: BRDFInputRecord -> Expr BRDFInput
toBRDFInput { view, normal, light } = view `tuple` (normal `tuple` (light.direction `tuple` light.intensity))

fromBRDFInput :: Expr BRDFInput -> BRDFInputRecord
fromBRDFInput info = { view, normal, light }
  where
    (Tuple view   z0) = from info
    (Tuple normal z1) = from z0
    (Tuple dir int)   = from z1
    light = { direction: dir, intensity: int }

sample :: BRDF -> BRDFInputRecord -> Builder (Expr Color)
sample brdf input = unwrap brdf $ toBRDFInput input


-- BRDFs
diffuseBRDF :: Color -> BRDF
diffuseBRDF albedo = wrap \info ->
  let
    { view, normal, light } = fromBRDFInput info
    diff = ((normal <.> light.direction) *^ light.intensity) * (cast albedo)
  in
    pure $ diff

emissiveBRDF :: Color -> BRDF
emissiveBRDF color = wrap $ const $ pure (cast color)

specularBRDF :: Number -> BRDF
specularBRDF shininess = wrap \info ->
  let
    { view, normal: n, light } = fromBRDFInput info
    half = normalized $ light.direction ^+^ view
    fac  = saturate $ n <.> half
    int  = pow fac (cast shininess)
    out  = int *^ light.intensity
  in
    pure out
