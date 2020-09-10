module Test.Main where

import Prelude

import Data.Foldable (intercalate)
import Effect (Effect)
import Effect.Class.Console (log)
import Shader.Expr (BinaryExpr(..), Expr(..), bindE, inl, matchE, num)
import Shader.Expr.Normalization (normalize)

x :: Expr Number
x = EVar "x"

y :: Expr Number
y = EVar "y"

z :: Expr Number
z = EVar "z"

w :: Expr Number
w = EVar "w"

expr :: Expr Number
expr = matchE
  (bindE "w"
    (num 0.0)
    (bindE "x" (num 0.0) (inl w)))
  ""
  (const (num 0.0))
  (const (num 1.0))

main :: Effect Unit
main = do
  log $ showExpr e
  log $ showExpr $ e'
  where 
    e  = expr
    e' = normalize expr

showCtor :: String -> Array String -> String
showCtor ctor args = "(" <> ctor <> " " <> intercalate " " args <>  ")"

showExpr :: forall a. Expr a -> String
showExpr (EVar v)           = showCtor "EVar" $ [v]
showExpr (ENum n)           = showCtor "ENum" $ [show n]
showExpr (EBool b)          = showCtor "EBool" $ [show b]
showExpr (EBinary e)        = showCtor "EBinary" $ [showBinaryExpr e]
showExpr (EIf i t e)        = showCtor "EIf" $ [showExpr i, showExpr t, showExpr e]
showExpr (ETuple e1 e2)     = showCtor "ETuple" $ [showExpr e1, showExpr e2]
showExpr (EFst e)           = showCtor "EFst" $ [showExpr e]
showExpr (ESnd e)           = showCtor "ESnd" $ [showExpr e]
showExpr (EInl e)           = showCtor "EInl" $ [showExpr e]
showExpr (EInr e)           = showCtor "EInr" $ [showExpr e]
showExpr (EMatch e lname l rname r) = showCtor "EMatch" $ [showExpr e, lname, showExpr l, rname, showExpr r]
showExpr (EBind v ty e1 e2)         = showCtor "EBind" $ [v, showExpr e1, showExpr e2]
showExpr _ = ""

showBinaryExpr :: forall a. BinaryExpr a -> String
showBinaryExpr (BinPlus e1 e2) = showCtor "EPlus" $ showExpr <$> [e1, e2]
showBinaryExpr _ = ""
