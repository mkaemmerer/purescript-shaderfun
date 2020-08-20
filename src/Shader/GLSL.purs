module Shader.GLSL (toGLSL) where

import Prelude
import Data.Foldable (intercalate)
import Shader.Expr (ShaderExpr(..), UnaryOp(..), BinaryOp(..))

-- Code generation
toGLSL :: forall a. ShaderExpr a -> String
toGLSL = fixPrecedence >>> printExpr

printList :: Array String -> String
printList = intercalate ", "

printExpr :: forall a. ShaderExpr a -> String
printExpr (EVar name)          = name
printExpr (ENum n proof)       = show n
printExpr (EBool b proof)      = show b
printExpr (EVec2 x y proof)    = "vec2(" <> printList [ printExpr x, printExpr y ] <> ")"
printExpr (EColor r g b proof) = "vec3(" <> printList [ printExpr r, printExpr g, printExpr b ] <> ")"
printExpr (EUnary op e)        = printUnary op e
printExpr (EBinary op l r)     = printBinary op l r
printExpr (EParen e)           = "(" <> printExpr e <> ")"
printExpr (ECall fn args)      = fn <> "(" <> printList (printExpr <$> args) <> ")"

printUnary :: forall a. UnaryOp -> ShaderExpr a -> String
printUnary OpNegate e = "-" <> printExpr e
printUnary OpNot    e = "!" <> printExpr e
printUnary OpProjX  e = printExpr e <> ".x"
printUnary OpProjY  e = printExpr e <> ".y"
printUnary OpProjR  e = printExpr e <> ".r"
printUnary OpProjG  e = printExpr e <> ".g"
printUnary OpProjB  e = printExpr e <> ".b"

printBinary :: forall a. BinaryOp -> ShaderExpr a -> ShaderExpr a -> String
printBinary op l r = intercalate " " [ printExpr l, printBinaryOp op, printExpr r ]
  where
    printBinaryOp OpEq      = "=="
    printBinaryOp OpNeq     = "!="
    printBinaryOp OpAnd     = "&&"
    printBinaryOp OpOr      = "||"
    printBinaryOp OpPlus    = "+"
    printBinaryOp OpMinus   = "-"
    printBinaryOp OpTimes   = "*"
    printBinaryOp OpDiv     = "/"
    printBinaryOp OpLt      = "<"
    printBinaryOp OpLte     = "<="
    printBinaryOp OpGt      = ">"
    printBinaryOp OpGte     = ">="
    printBinaryOp OpPlusV   = "+"
    printBinaryOp OpMinusV  = "-"
    printBinaryOp OpScaleV  = "*"
    printBinaryOp OpPlusC   = "+"
    printBinaryOp OpMinusC  = "-"
    printBinaryOp OpTimesC  = "*"
    printBinaryOp OpScaleC  = "*"

-- Normalization 
topPrec :: Int
topPrec = 100

unaryPrecedence :: UnaryOp -> Int
unaryPrecedence OpNegate = 1
unaryPrecedence OpNot    = 1
unaryPrecedence OpProjX  = 2
unaryPrecedence OpProjY  = 2
unaryPrecedence OpProjR  = 2
unaryPrecedence OpProjG  = 2
unaryPrecedence OpProjB  = 2

binaryPrecedence :: BinaryOp -> Int
binaryPrecedence OpTimes   = 3
binaryPrecedence OpDiv     = 3
binaryPrecedence OpScaleV  = 4
binaryPrecedence OpTimesC  = 4
binaryPrecedence OpScaleC  = 4
binaryPrecedence OpPlus    = 5
binaryPrecedence OpMinus   = 5
binaryPrecedence OpPlusV   = 6
binaryPrecedence OpMinusV  = 6
binaryPrecedence OpPlusC   = 6
binaryPrecedence OpMinusC  = 6
binaryPrecedence OpLt      = 7
binaryPrecedence OpLte     = 7
binaryPrecedence OpGt      = 7
binaryPrecedence OpGte     = 7
binaryPrecedence OpEq      = 8
binaryPrecedence OpNeq     = 8
binaryPrecedence OpAnd     = 9
binaryPrecedence OpOr      = 10

fixPrecedence :: forall a. ShaderExpr a -> ShaderExpr a
fixPrecedence e = fixPrecedence' topPrec e

fixPrecedence' :: forall a. Int -> ShaderExpr a -> ShaderExpr a
fixPrecedence' prec (EVar name)          = EVar name
fixPrecedence' prec (ENum n proof)       = ENum n proof
fixPrecedence' prec (EBool b proof)      = EBool b proof
fixPrecedence' prec (EVec2 x y proof)    = EVec2 (fixPrecedence' topPrec x) (fixPrecedence' topPrec y) proof
fixPrecedence' prec (EColor r g b proof) = EColor (fixPrecedence' topPrec r) (fixPrecedence' topPrec g) (fixPrecedence' topPrec b) proof
fixPrecedence' prec (EUnary op e)        = if opPrec < prec then inner else EParen (inner)
  where
    opPrec = unaryPrecedence op
    inner = EUnary op (fixPrecedence' opPrec e)
fixPrecedence' prec (EBinary op l r)     = if opPrec < prec then inner else EParen (inner)
  where
    opPrec = binaryPrecedence op
    inner = EBinary op (fixPrecedence' opPrec l) (fixPrecedence' opPrec r)
fixPrecedence' prec (EParen e)           = EParen (fixPrecedence' topPrec e)
fixPrecedence' prec (ECall fn args)      = ECall fn (fixPrecedence' topPrec <$> args)