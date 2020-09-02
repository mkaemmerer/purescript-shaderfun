module Shader.GLSL (toGLSL) where

import Prelude

import Data.Color (Color)
import Partial.Unsafe (unsafePartial)
import Shader.Expr (Expr)
import Shader.Expr.Normalization (normalize)
import Shader.Expr.Optimization (class ConstantFold, constantFold, class IdentityFold, identityFold)
import Shader.GLSL.CST (fromExpr, printCST)

-- | Convert an expression to GLSL code
toGLSL :: Expr Color -> String
toGLSL = unsafePartial $ normalize >>> optimize >>> printExpr

printExpr :: Partial => forall a. Expr a -> String
printExpr = fromExpr >>> printCST

optimize :: forall a. (ConstantFold a) => (IdentityFold a) => Expr a -> Expr a
optimize = constantFold >>> identityFold
