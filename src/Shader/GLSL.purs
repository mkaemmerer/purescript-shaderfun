module Shader.GLSL (toGLSL) where

import Prelude

import Data.Color (Color)
import Data.Foldable (intercalate)
import Partial (crash)
import Partial.Unsafe (unsafePartial)
import Shader.Expr (BinaryExpr(..), CallExpr(..), Expr(..), Type(..), UnaryExpr(..))
import Shader.Expr.Normalization (normalize)
import Shader.Expr.Optimization (class ConstantFold, constantFold, class IdentityFold, identityFold)
import Shader.Expr.Traversal (exl, exr, ex)

-- | Convert an expression to GLSL code
toGLSL :: Expr Color -> String
toGLSL = unsafePartial $ normalize >>> optimize >>> printExpr

printList :: Array String -> String
printList = intercalate ", "

-- Partial functions reference the fact that not all expressions in the grammar are well typed.
-- Should (knock on wood) still be a total functions over well typed expressions

printExpr :: Partial => forall a. Expr a -> String
printExpr = printExprWithReturn

printExprWithReturn :: Partial => forall a. Expr a -> String
printExprWithReturn (EBind name ty EUnit body) =
  printType ty <> " " <> name <> ";\n" <> printExprWithReturn body
printExprWithReturn (EBind name ty val body) =
  printType ty <> " " <> name <> " = " <> printExpr' topPrec val <> ";\n" <> printExprWithReturn body
printExprWithReturn e =
  "return " <> printExpr' topPrec e <> ";"

printExpr' :: Partial => forall a. Int -> Expr a -> String
printExpr' p (EVar name)              = name
printExpr' p (ENum n)                 = show n
printExpr' p (EBool b)                = show b
printExpr' p (EVec2 x y)              = "vec2(" <> printList (printExpr' topPrec <$> [ x, y ]) <> ")"
printExpr' p (EVec3 x y z)            = "vec3(" <> printList (printExpr' topPrec <$> [ x, y, z ]) <> ")"
printExpr' p (EComplex r i)           = "vec2(" <> printList (printExpr' topPrec <$> [ r, i ]) <> ")"
printExpr' p (EColor r g b)           = "vec3(" <> printList (printExpr' topPrec <$> [ r, g, b ]) <> ")"
printExpr' p (EUnary e)               = printUnary p e
printExpr' p (EBinary e)              = printBinary p e
printExpr' p (ECall e)                = printCall topPrec e
printExpr' p (EIf i t e)              = printIf p i t e
-- When unit is used as a sentinel value, don't initialize to anything
printExpr' p (EBind name ty EUnit body) = printType ty <> " " <> name <> ";\n" <> printExpr' topPrec body
printExpr' p (EBind name ty val body)   = printType ty <> " " <> name <> " = " <> printExpr' topPrec val <> ";\n" <> printExpr' topPrec body
-- Not handled
printExpr' p (ETuple _ _) = crash
printExpr' p (EFst _) = crash
printExpr' p (ESnd _) = crash
printExpr' p (EUnit) = crash

printType :: Type -> String
printType TBoolean = "bool"
printType TScalar  = "float"
printType TVec2    = "vec2"
printType TVec3    = "vec3"
printType TComplex = "vec2"
printType TColor   = "vec3"

printIf :: Partial => forall a. Int -> Expr Boolean -> Expr a -> Expr a -> String
printIf p i t e = if p < ifPrec
    then "(" <> printIf' topPrec <> ")"
    else printIf' ifPrec
  where
    printIf' prec = printExpr' prec i <> " ? " <> printExpr' prec t <> " : " <> printExpr' prec e

printUnary :: Partial => forall a. Int -> UnaryExpr a -> String
printUnary p unary = if p < opPrec
    then printUnaryOp unary $ "(" <> printExpr' topPrec (ex unary) <> ")"
    else printUnaryOp unary $ printExpr' opPrec (ex unary)
  where
    opPrec = unaryPrec unary
    printUnaryOp (UnNegate e)         str = "-" <> str
    printUnaryOp (UnNot e)            str = "!" <> str
    printUnaryOp (UnProjV2X e)        str = str <> ".x"
    printUnaryOp (UnProjV2Y e)        str = str <> ".y"
    printUnaryOp (UnProjV3X e)        str = str <> ".x"
    printUnaryOp (UnProjV3Y e)        str = str <> ".y"
    printUnaryOp (UnProjV3Z e)        str = str <> ".z"
    printUnaryOp (UnProjR e)          str = str <> ".r"
    printUnaryOp (UnProjG e)          str = str <> ".g"
    printUnaryOp (UnProjB e)          str = str <> ".b"
    printUnaryOp (UnProjReal e)       str = str <> ".x"
    printUnaryOp (UnProjImaginary e)  str = str <> ".y"

printBinary :: Partial => forall a. Int -> BinaryExpr a -> String
printBinary p binary = if p < opPrec
    then "(" <> printBinary' topPrec <> ")"
    else printBinary' opPrec
  where
    printBinary' prec = intercalate " " [ printExpr' prec (exl binary), printBinaryOp binary, printExpr' prec (exr binary) ]
    opPrec = binaryPrec binary
    printBinaryOp (BinEq _ _)        = "=="
    printBinaryOp (BinNeq _ _)       = "!="
    printBinaryOp (BinAnd _ _)       = "&&"
    printBinaryOp (BinOr _ _)        = "||"
    printBinaryOp (BinLt _ _)        = "<"
    printBinaryOp (BinLte _ _)       = "<="
    printBinaryOp (BinGt _ _)        = ">"
    printBinaryOp (BinGte _ _)       = ">="
    printBinaryOp (BinPlus _ _)      = "+"
    printBinaryOp (BinMinus _ _)     = "-"
    printBinaryOp (BinTimes _ _)     = "*"
    printBinaryOp (BinDiv _ _)       = "/"
    printBinaryOp (BinPlusV2 _ _)    = "+"
    printBinaryOp (BinMinusV2 _ _)   = "-"
    printBinaryOp (BinScaleV2 _ _)   = "*"
    printBinaryOp (BinPlusV3 _ _)    = "+"
    printBinaryOp (BinMinusV3 _ _)   = "-"
    printBinaryOp (BinScaleV3 _ _)   = "*"
    printBinaryOp (BinPlusC _ _)     = "+"
    printBinaryOp (BinMinusC _ _)    = "-"
    printBinaryOp (BinScaleC _ _)    = "*"
    printBinaryOp (BinPlusCol _ _)   = "+"
    printBinaryOp (BinMinusCol _ _)  = "-"
    printBinaryOp (BinTimesCol _ _)  = "*"
    printBinaryOp (BinScaleCol _ _)  = "*"
    printBinaryOp (BinDivC _ _)   = crash -- GLSL has no builtin operation. Handled by "elaborating"
    printBinaryOp (BinTimesC _ _) = crash -- GLSL has no builtin operation. Handled by "elaborating"

printCall :: Partial => forall a. Int -> CallExpr a -> String
printCall p (FnAbs e)               = "abs" <> "(" <> printList [printExpr' p e] <> ")"
printCall p (FnCos e)               = "cos" <> "(" <> printList [printExpr' p e] <> ")"
printCall p (FnFloor e)             = "floor" <> "(" <> printList [printExpr' p e] <> ")"
printCall p (FnFract e)             = "fract" <> "(" <> printList [printExpr' p e] <> ")"
printCall p (FnLog e)               = "log" <> "(" <> printList [printExpr' p e] <> ")"
printCall p (FnLog2 e)              = "log2" <> "(" <> printList [printExpr' p e] <> ")"
printCall p (FnSaturate e)          = "saturate" <> "(" <> printList [printExpr' p e] <> ")"
printCall p (FnSin e)               = "sin" <> "(" <> printList [printExpr' p e] <> ")"
printCall p (FnSqrt e)              = "sqrt" <> "(" <> printList [printExpr' p e] <> ")"
printCall p (FnAtan e1 e2)          = "atan" <> "(" <> printList [printExpr' p e1, printExpr' p e2] <> ")"
printCall p (FnMax e1 e2)           = "max" <> "(" <> printList [printExpr' p e1, printExpr' p e2] <> ")"
printCall p (FnMin e1 e2)           = "min" <> "(" <> printList [printExpr' p e1, printExpr' p e2] <> ")"
printCall p (FnMod e1 e2)           = "mod" <> "(" <> printList [printExpr' p e1, printExpr' p e2] <> ")"
printCall p (FnSmoothstep e1 e2 e3) = "smoothstep" <> "(" <> printList [printExpr' p e1, printExpr' p e2, printExpr' p e3] <> ")"
printCall p (FnLengthV2 e)          = "length" <> "(" <> printList [printExpr' p e] <> ")"
printCall p (FnLengthV3 e)          = "length" <> "(" <> printList [printExpr' p e] <> ")"
printCall p (FnDotV2 e1 e2)         = "dot" <> "(" <> printList [printExpr' p e1, printExpr' p e2] <> ")"
printCall p (FnDotV3 e1 e2)         = "dot" <> "(" <> printList [printExpr' p e1, printExpr' p e2] <> ")"
printCall p (FnDotC e1 e2)          = "dot" <> "(" <> printList [printExpr' p e1, printExpr' p e2] <> ")"
printCall p (FnReflectV2 e1 e2)     = "reflect" <> "(" <> printList [printExpr' p e1, printExpr' p e2] <> ")"
printCall p (FnReflectV3 e1 e2)     = "reflect" <> "(" <> printList [printExpr' p e1, printExpr' p e2] <> ")"
printCall p (FnMix e1 e2 e3)        = "mix" <> "(" <> printList [printExpr' p e1, printExpr' p e2, printExpr' p e3] <> ")"


topPrec :: Int
topPrec = 100

unaryPrec :: forall a. UnaryExpr a -> Int
unaryPrec (UnNegate _)   = 1
unaryPrec (UnNot _)      = 1
unaryPrec (UnProjV2X _)  = 2
unaryPrec (UnProjV2Y _)  = 2
unaryPrec (UnProjV3X _)  = 2
unaryPrec (UnProjV3Y _)  = 2
unaryPrec (UnProjV3Z _)  = 2
unaryPrec (UnProjR _)    = 2
unaryPrec (UnProjG _)    = 2
unaryPrec (UnProjB _)    = 2
unaryPrec (UnProjReal _)      = 2
unaryPrec (UnProjImaginary _) = 2

binaryPrec :: forall a. BinaryExpr a -> Int
binaryPrec (BinTimes _ _)     = 3
binaryPrec (BinDiv _ _)       = 3
binaryPrec (BinScaleV2 _ _)   = 4
binaryPrec (BinScaleV3 _ _)   = 4
binaryPrec (BinScaleC _ _)    = 4
binaryPrec (BinTimesC _ _)    = 4
binaryPrec (BinDivC _ _)      = 4
binaryPrec (BinScaleCol _ _)  = 4
binaryPrec (BinTimesCol _ _)  = 4
binaryPrec (BinPlus _ _)      = 5
binaryPrec (BinPlusV2 _ _)    = 6
binaryPrec (BinPlusV3 _ _)    = 6
binaryPrec (BinPlusC _ _)     = 6
binaryPrec (BinPlusCol _ _)   = 6
binaryPrec (BinMinus _ _)     = 5
binaryPrec (BinMinusV2 _ _)   = 6
binaryPrec (BinMinusV3 _ _)   = 6
binaryPrec (BinMinusC _ _)    = 6
binaryPrec (BinMinusCol _ _)  = 6
binaryPrec (BinLt _ _)        = 7
binaryPrec (BinLte _ _)       = 7
binaryPrec (BinGt _ _)        = 7
binaryPrec (BinGte _ _)       = 7
binaryPrec (BinEq _ _)        = 8
binaryPrec (BinNeq _ _)       = 8
binaryPrec (BinAnd _ _)       = 9
binaryPrec (BinOr _ _)        = 10

ifPrec :: Int
ifPrec = 11

-- Optimizations
optimize :: forall a. (ConstantFold a) => (IdentityFold a) => Expr a -> Expr a
optimize = constantFold >>> identityFold
