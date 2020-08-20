module Example.Circle (source) where

import Prelude

import Data.Color (Color(..))
import Shader.Expr (Expr, fromColor, ifE, length, lt, num)
import Shader.ExprBuilder (decl)
import Shader.Function (ShaderProgram, runShaderProgram)
import Shader.GLSL (toGLSL)

white :: Expr Color
white = fromColor $ Color 1.0 1.0 1.0

black :: Expr Color
black = fromColor $ Color 0.0 0.0 0.0

program :: ShaderProgram
program p = do
  len <- decl $ length p
  col <- decl $ ifE (lt len (num 600.0)) white black
  pure col

source :: String
source = toGLSL $ runShaderProgram program