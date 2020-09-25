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
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Vec2 (Vec2)
import Data.VectorSpace (class AdditiveGroup, class InnerSpace, class VectorSpace, addV, dot, negateV, scale, subV, zeroV)
import Shader.Expr (Expr, p)
import Shader.ExprBuilder (Builder, runExprBuilder)

newtype BuilderFunc a b = BuilderFunc (a -> Builder b)
derive instance newtypeBuilderFunc :: Newtype (BuilderFunc a b) _

type ShaderProgram a b = BuilderFunc (Expr a) (Expr b)
infixr 4 type ShaderProgram as :>

shaderProgram :: forall a b. ((Expr a) -> Builder (Expr b)) -> ShaderProgram a b
shaderProgram = wrap

type BFunc a b = a -> Builder b
over2 ::
  forall a b c d e f.
  (BFunc a b -> BFunc c d -> BFunc e f) ->
  (BuilderFunc a b -> BuilderFunc c d -> BuilderFunc e f)
over2 f x y = wrap $ f (unwrap x) (unwrap y)

runShaderProgram :: ShaderProgram Vec2 Color -> Expr Color
runShaderProgram program = runExprBuilder (unwrap program $ p)


-- Function type class instances
instance semigroupoidBuilderFunc :: Semigroupoid BuilderFunc where
  compose = over2 composeKleisliFlipped

instance categoryBuilderFunc :: Category BuilderFunc where
  identity = wrap $ identity >>> pure

instance functorBuilderFunc :: Functor (BuilderFunc a) where
  map f = over BuilderFunc $ map (map f)

instance applyBuilderFunc :: Apply (BuilderFunc a) where
  apply = over2 $ \f g x -> apply (f x) (g x)

instance applicativeBuilderFunc :: Applicative (BuilderFunc a) where
  pure x = wrap $ pure $ pure x


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
