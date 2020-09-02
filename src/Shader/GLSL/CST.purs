module Shader.GLSL.CST (CST, printCST, fromExpr) where

import Prelude

import Data.Foldable (intercalate)
import Partial (crash)
import Shader.Expr (BinaryExpr(..), CallExpr(..), Expr(..), Type(..), UnaryExpr(..))

type UnaryOp  = String
type BinaryOp = String
type GLSLFn   = String
type GLSLType = String

data CST
  = CVar String
  | CBool Boolean
  | CFloat Number
  | CVec2 CST CST
  | CVec3 CST CST CST
  | CPrefix UnaryOp CST
  | CPostfix CST UnaryOp
  | CBinary BinaryOp CST CST
  | CCall GLSLFn (Array CST)
  | CIf CST CST CST
  | CParen CST
  | CReturn CST
  | CBind GLSLType String CST CST
  | CDecl GLSLType String CST -- declare variable without initialization


printCST :: CST -> String
printCST (CVar n)             = n
printCST (CBool b)            = show b
printCST (CFloat n)           = show n
printCST (CVec2 x y)          = "vec2(" <> printList [x, y] <> ")"
printCST (CVec3 x y z)        = "vec3(" <> printList [x, y, z] <> ")"
printCST (CPrefix op e)       = op <> printCST e
printCST (CPostfix e op)      = printCST e <> op
printCST (CBinary op l r)     = intercalate " " [printCST l, op, printCST r]
printCST (CCall fn args)      = fn <> "(" <> printList args <> ")"
printCST (CIf i t e)          = printCST i <> " ? " <> printCST t <> " : " <> printCST e
printCST (CParen e)           = "(" <> printCST e <> ")"
printCST (CBind t name e1 e2) = t <> " " <> name <> " = " <> printCST e1 <> ";\n" <> printCST e2
printCST (CDecl t name e)     = t <> " " <> name <> ";\n" <> printCST e
printCST (CReturn e)          = "return " <> printCST e <> ";"

printList :: Array CST -> String
printList l = l # map printCST >>> intercalate ", "



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

maybeParens :: Int -> Int -> (Int -> CST) -> CST
maybeParens p1 p2 mkCST = if p1 < p2 then CParen (mkCST topPrec) else mkCST p2


-- Partial functions reference the fact that not all expressions in the grammar are well typed.
-- Should (knock on wood) still be a total functions over well typed expressions
fromExpr :: Partial => forall a. Expr a -> CST
fromExpr e = fromExprPrec topPrec e

fromExprPrec :: Partial => forall a. Int -> Expr a -> CST
fromExprPrec p (EVar name)    = CVar name
fromExprPrec p (ENum n)       = CFloat n
fromExprPrec p (EBool b)      = CBool b
fromExprPrec p (EVec2 x y)    = CVec2 (fromExpr x) (fromExpr y)
fromExprPrec p (EVec3 x y z)  = CVec3 (fromExpr x) (fromExpr y) (fromExpr z)
fromExprPrec p (EComplex r i) = CVec2 (fromExpr r) (fromExpr i)
fromExprPrec p (EColor r g b) = CVec3 (fromExpr r) (fromExpr g) (fromExpr b)
fromExprPrec p (EUnary e)     = maybeParens p opPrec mkUnary
  where
    opPrec = unaryPrec e
    mkUnary q = fromUnaryExpr q e
fromExprPrec p (EBinary e)    = maybeParens p opPrec mkBinary
  where
    opPrec = binaryPrec e
    mkBinary q = fromBinaryExpr q e
fromExprPrec p (ECall e)      = fromCallExpr e
fromExprPrec p (EIf i t e)    = maybeParens p ifPrec mkIf
  where
    mkIf q= CIf (fromExprPrec q i) (fromExprPrec q t) (fromExprPrec q e)
fromExprPrec p (EBind v ty EUnit e) = CDecl (fromType ty) v (fromExpr e)
fromExprPrec p (EBind v ty e1 e2)   = CBind (fromType ty) v (fromExpr e1) (fromExprBody e2)
  where
    fromExprBody e@(EBind _ _ _ _) = fromExpr e
    fromExprBody e = CReturn (fromExpr e)
-- No concrete representation for these types. Handle by elaborating
fromExprPrec p (ETuple _ _) = crash
fromExprPrec p (EFst _)     = crash
fromExprPrec p (ESnd _)     = crash
fromExprPrec p (EUnit)      = crash

fromType :: Type -> String
fromType TBoolean = "bool"
fromType TScalar  = "float"
fromType TVec2    = "vec2"
fromType TVec3    = "vec3"
fromType TComplex = "vec2"
fromType TColor   = "vec3"

fromUnaryExpr :: Partial => forall a. Int -> UnaryExpr a -> CST
fromUnaryExpr p (UnNegate e)         = CPrefix "-" (fromExprPrec p e)
fromUnaryExpr p (UnNot e)            = CPrefix "!" (fromExprPrec p e)
fromUnaryExpr p (UnProjV2X e)        = CPostfix (fromExprPrec p e) ".x"
fromUnaryExpr p (UnProjV2Y e)        = CPostfix (fromExprPrec p e) ".y"
fromUnaryExpr p (UnProjV3X e)        = CPostfix (fromExprPrec p e) ".x"
fromUnaryExpr p (UnProjV3Y e)        = CPostfix (fromExprPrec p e) ".y"
fromUnaryExpr p (UnProjV3Z e)        = CPostfix (fromExprPrec p e) ".z"
fromUnaryExpr p (UnProjR e)          = CPostfix (fromExprPrec p e) ".r"
fromUnaryExpr p (UnProjG e)          = CPostfix (fromExprPrec p e) ".g"
fromUnaryExpr p (UnProjB e)          = CPostfix (fromExprPrec p e) ".b"
fromUnaryExpr p (UnProjReal e)       = CPostfix (fromExprPrec p e) ".x"
fromUnaryExpr p (UnProjImaginary e)  = CPostfix (fromExprPrec p e) ".y"

fromBinaryExpr :: Partial => forall a. Int -> BinaryExpr a -> CST
fromBinaryExpr p (BinEq e1 e2)        = CBinary "==" (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinNeq e1 e2)       = CBinary "!=" (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinAnd e1 e2)       = CBinary "&&" (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinOr e1 e2)        = CBinary "||" (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinLt e1 e2)        = CBinary "<"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinLte e1 e2)       = CBinary "<=" (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinGt e1 e2)        = CBinary ">"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinGte e1 e2)       = CBinary ">=" (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinPlus e1 e2)      = CBinary "+"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinMinus e1 e2)     = CBinary "-"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinTimes e1 e2)     = CBinary "*"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinDiv e1 e2)       = CBinary "/"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinPlusV2 e1 e2)    = CBinary "+"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinMinusV2 e1 e2)   = CBinary "-"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinScaleV2 e1 e2)   = CBinary "*"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinPlusV3 e1 e2)    = CBinary "+"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinMinusV3 e1 e2)   = CBinary "-"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinScaleV3 e1 e2)   = CBinary "*"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinPlusC e1 e2)     = CBinary "+"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinMinusC e1 e2)    = CBinary "-"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinScaleC e1 e2)    = CBinary "*"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinPlusCol e1 e2)   = CBinary "+"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinMinusCol e1 e2)  = CBinary "-"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinTimesCol e1 e2)  = CBinary "*"  (fromExprPrec p e1) (fromExprPrec p e2)
fromBinaryExpr p (BinScaleCol e1 e2)  = CBinary "*"  (fromExprPrec p e1) (fromExprPrec p e2)
-- Handle these ops by elaborating
fromBinaryExpr p (BinDivC _ _)        = crash
fromBinaryExpr p (BinTimesC _ _)      = crash

fromCallExpr :: Partial => forall a. CallExpr a -> CST
fromCallExpr (FnAbs e)               = CCall "abs"        $ [fromExpr e]
fromCallExpr (FnCos e)               = CCall "cos"        $ [fromExpr e]
fromCallExpr (FnFloor e)             = CCall "floor"      $ [fromExpr e]
fromCallExpr (FnFract e)             = CCall "fract"      $ [fromExpr e]
fromCallExpr (FnLog e)               = CCall "log"        $ [fromExpr e]
fromCallExpr (FnLog2 e)              = CCall "log2"       $ [fromExpr e]
fromCallExpr (FnSaturate e)          = CCall "saturate"   $ [fromExpr e]
fromCallExpr (FnSin e)               = CCall "sin"        $ [fromExpr e]
fromCallExpr (FnSqrt e)              = CCall "sqrt"       $ [fromExpr e]
fromCallExpr (FnAtan e1 e2)          = CCall "atan"       $ [fromExpr e1, fromExpr e2]
fromCallExpr (FnMax e1 e2)           = CCall "max"        $ [fromExpr e1, fromExpr e2]
fromCallExpr (FnMin e1 e2)           = CCall "min"        $ [fromExpr e1, fromExpr e2]
fromCallExpr (FnMod e1 e2)           = CCall "mod"        $ [fromExpr e1, fromExpr e2]
fromCallExpr (FnSmoothstep e1 e2 e3) = CCall "smoothstep" $ [fromExpr e1, fromExpr e2, fromExpr e3]
fromCallExpr (FnLengthV2 e)          = CCall "length"     $ [fromExpr e]
fromCallExpr (FnLengthV3 e)          = CCall "length"     $ [fromExpr e]
fromCallExpr (FnDotV2 e1 e2)         = CCall "dot"        $ [fromExpr e1, fromExpr e2]
fromCallExpr (FnDotV3 e1 e2)         = CCall "dot"        $ [fromExpr e1, fromExpr e2]
fromCallExpr (FnDotC e1 e2)          = CCall "dot"        $ [fromExpr e1, fromExpr e2]
fromCallExpr (FnReflectV2 e1 e2)     = CCall "reflect"    $ [fromExpr e1, fromExpr e2]
fromCallExpr (FnReflectV3 e1 e2)     = CCall "reflect"    $ [fromExpr e1, fromExpr e2]
fromCallExpr (FnMix e1 e2 e3)        = CCall "mix"        $ [fromExpr e1, fromExpr e2, fromExpr e3]
