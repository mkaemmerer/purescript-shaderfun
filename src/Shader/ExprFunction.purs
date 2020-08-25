module Shader.ExprFunction
  ( shaderProgram
  , runShaderProgram
  , BuilderFunc
  , ShaderProgram
  ) where

import Prelude
import Control.Bind (composeKleisliFlipped)
import Data.Color (Color)
import Data.Vec2 (Vec2)
import Shader.Expr (Expr, p)
import Shader.ExprBuilder (Builder, runExprBuilder)

type BFunc a b = a -> Builder b
data BuilderFunc a b = BuilderFunc (BFunc a b)
type ShaderProgram a b = BuilderFunc (Expr a) (Expr b)

shaderProgram :: forall a b. BFunc (Expr a) (Expr b) -> ShaderProgram a b
shaderProgram f = BuilderFunc f

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

runShaderProgram :: ShaderProgram Vec2 Color -> Expr Color
runShaderProgram program = runExprBuilder (unwrapBuilder program $ p)
