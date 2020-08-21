module Shader.Function (ShaderFunc, ShaderProgram, runShaderProgram) where

import Data.Color (Color)
import Data.Vec2 (Vec2)
import Shader.Expr (Expr, p)
import Shader.ExprBuilder (ExprBuilder, runExprBuilder)

-- TODO: newtype wrapper??
type ShaderFunc a b = Expr a -> ExprBuilder b
type ShaderProgram = ShaderFunc Vec2 Color

runShaderProgram :: ShaderProgram -> Expr Color
runShaderProgram program = runExprBuilder (program p)
