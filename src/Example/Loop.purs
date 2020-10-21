module Example.Loop (source) where

import Prelude

import Data.Color (Color)
import Data.HeytingAlgebra (ff)
import Data.Tuple (Tuple)
import Data.Vec2 (Vec2)
import Shader.Expr (Expr, done, fst, ifE, loop, snd, tuple)
import Shader.ExprBuilder (rec)
import Shader.ExprFunction (ShaderProgram, runShaderProgram, shaderProgram)
import Shader.GLSL (toGLSL)

start :: Expr (Tuple Color Boolean)
start = tuple one ff

addOne :: Expr (Tuple Color Boolean) -> Expr (Tuple Color Boolean)
addOne x = tuple (one + fst x) (snd x)

program :: ShaderProgram Vec2 Color
program = shaderProgram $ \v -> do
  let step x = pure $ ifE (snd x) (done x) (loop $ addOne x)
  t <- rec 1 step start
  pure $ (fst t) + one

source :: String
source = toGLSL $ runShaderProgram program
