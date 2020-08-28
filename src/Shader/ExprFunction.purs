module Shader.ExprFunction
  ( shaderProgram
  , runShaderProgram
  , BuilderFunc
  , ShaderProgram
  , type (:>)
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Bind (composeKleisliFlipped)
import Data.Color (Color)
import Data.Vec2 (Vec2)
import Data.VectorSpace (class AdditiveGroup, class InnerSpace, class VectorSpace, addV, dot, negateV, scale, subV, zeroV)
import Shader.Expr (Expr, p)
import Shader.ExprBuilder (Builder, runExprBuilder)

type BFunc a b = a -> Builder b
data BuilderFunc a b = BuilderFunc (BFunc a b)
type ShaderProgram a b = BuilderFunc (Expr a) (Expr b)

infixr 4 type ShaderProgram as :>

shaderProgram :: forall a b. BFunc (Expr a) (Expr b) -> ShaderProgram a b
shaderProgram = wrapBuilder

wrapBuilder :: forall a b. BFunc a b -> BuilderFunc a b
wrapBuilder f = BuilderFunc f

unwrapBuilder :: forall a b. BuilderFunc a b -> (BFunc a b)
unwrapBuilder (BuilderFunc f) = f

over ::
  forall a b c d.
  (BFunc a b -> BFunc c d) ->
  (BuilderFunc a b -> BuilderFunc c d)
over f x = wrapBuilder $ f (unwrapBuilder x)

over2 ::
  forall a b c d e f.
  (BFunc a b -> BFunc c d -> BFunc e f) ->
  (BuilderFunc a b -> BuilderFunc c d -> BuilderFunc e f)
over2 f x y = wrapBuilder $ f (unwrapBuilder x) (unwrapBuilder y)

runShaderProgram :: ShaderProgram Vec2 Color -> Expr Color
runShaderProgram program = runExprBuilder (unwrapBuilder program $ p)


-- Function type class instances
instance semigroupoidBuilderFunc :: Semigroupoid BuilderFunc where
  compose = over2 composeKleisliFlipped

instance categoryBuilderFunc :: Category BuilderFunc where
  identity = wrapBuilder $ identity >>> pure

instance functorBuilderFunc :: Functor (BuilderFunc a) where
  map f = over $ map (map f)

instance applyBuilderFunc :: Apply (BuilderFunc a) where
  apply = over2 $ \f g x -> apply (f x) (g x)

instance applicativeBuilderFunc :: Applicative (BuilderFunc a) where
  pure x = wrapBuilder $ pure $ pure x


-- Numeric type class instances
instance semiringBuilderFunc :: (Semiring b) => Semiring (BuilderFunc a b) where
  zero = pure zero
  one = pure one
  add = lift2 add
  mul = lift2 mul

instance ringBuilderFunc :: (Ring b) => Ring (BuilderFunc a b) where
  sub = lift2 sub

instance commutativeRingBuilderFunc :: (CommutativeRing b) => CommutativeRing (BuilderFunc a b)

instance additiveGroupBuilderFunc :: (AdditiveGroup b) => AdditiveGroup (BuilderFunc a b) where
  zeroV = pure zeroV
  addV = lift2 addV
  subV = lift2 subV
  negateV = liftA1 negateV

instance euclideanRingBuilderFunc :: (EuclideanRing b) => EuclideanRing (BuilderFunc a b) where
  degree _ = 1
  mod = lift2 mod
  div = lift2 div

instance divisionRingBuilderFunc :: (DivisionRing b) => DivisionRing (BuilderFunc a b) where
  recip = liftA1 recip

instance vectorSpaceBuilderFunc :: (VectorSpace s v) => VectorSpace (BuilderFunc a s) (BuilderFunc a v) where
  scale = lift2 scale

instance innerSpaceBuilderFunc :: (InnerSpace s v) => InnerSpace (BuilderFunc a s) (BuilderFunc a v) where
  dot = lift2 dot
