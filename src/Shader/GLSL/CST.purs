module Shader.GLSL.CST (CExpr, CStmt, printCST, fromExpr) where

import Prelude

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.HeytingAlgebra (ff, tt)
import Data.Maybe (Maybe(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (unwrap)
import Data.String.Utils (startsWith)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Partial (crash)
import Shader.Expr (BinaryExpr(..), CallExpr(..), Expr(..), Type(..), UnaryExpr(..), matchE)
import Shader.Expr.Normalization (normalize)
import Shader.Expr.Traversal (foldVars)
import Unsafe.Coerce (unsafeCoerce)

type Var      = String
type UnaryOp  = String
type BinaryOp = String
type GLSLFn   = String
type GLSLType = String

data CStmt
  = CSDecl GLSLType Var CExpr
  | CSAssign Var CExpr
  | CSReturn CExpr
  | CSIf CExpr CBlock (Maybe CBlock)
  | CSLoop Int CBlock
  | CSBreak

type CBlock = Array CStmt

data CExpr
  = CEVar Var
  | CEBool Boolean
  | CEFloat Number
  | CEVec2 CExpr CExpr
  | CEVec2Shorthand CExpr
  | CEVec3 CExpr CExpr CExpr
  | CEVec3Shorthand CExpr
  | CEPrefix UnaryOp CExpr
  | CEPostfix UnaryOp CExpr
  | CEBinary BinaryOp CExpr CExpr
  | CECall GLSLFn (Array CExpr)
  | CEIf CExpr CExpr CExpr
  | CEParen CExpr

derive instance eqCExpr :: Eq CExpr
derive instance eqCStmt :: Eq CStmt

parens :: String -> String
parens s = "(" <> s <> ")"

brackets :: String -> String
brackets s = "{" <> "\n" <> s <> "\n" <> "}"

printCST :: CBlock -> String
printCST = printBlock

printBlock :: CBlock -> String
printBlock ss = intercalate "\n" $ printStmt <$> ss

printStmt :: CStmt -> String
printStmt (CSDecl ty name e)  = ty <> " " <> name <> " = " <> printExpr e <> ";"
printStmt (CSAssign name e)   = name <> " = " <> printExpr e <> ";"
printStmt (CSReturn e)        = "return " <> printExpr e <> ";"
printStmt (CSIf c t Nothing)  = "if" <> parens (printExpr c) <> brackets (printBlock t)
printStmt (CSIf c t (Just e)) = "if" <> parens (printExpr c) <> brackets (printBlock t) <> "else" <> brackets (printBlock e)
printStmt (CSLoop bound s)    = "\n" <> "for(int i=0; i<" <> (show bound) <> "; i++)" <> brackets (printBlock s) <> "\n"
printStmt (CSBreak)           = "break;"

printExpr :: CExpr -> String
printExpr (CEVar n)             = n
printExpr (CEBool b)            = show b
printExpr (CEFloat n)           = show n
printExpr (CEVec2 x y)          = "vec2(" <> printList [x, y] <> ")"
printExpr (CEVec2Shorthand xy)  = "vec2(" <> printList [xy] <> ")"
printExpr (CEVec3 x y z)        = "vec3(" <> printList [x, y, z] <> ")"
printExpr (CEVec3Shorthand xyz) = "vec3(" <> printList [xyz] <> ")"
printExpr (CEPrefix op e)       = op <> printExpr e
printExpr (CEPostfix op e)      = printExpr e <> op
printExpr (CEBinary op l r)     = intercalate " " [printExpr l, op, printExpr r]
printExpr (CECall fn args)      = fn <> "(" <> printList args <> ")"
printExpr (CEIf i t e)          = printExpr i <> " ? " <> printExpr t <> " : " <> printExpr e
printExpr (CEParen e)           = "(" <> printExpr e <> ")"

printList :: Array CExpr -> String
printList l = l # map printExpr >>> intercalate ", "


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

maybeParens :: Int -> Int -> (Int -> CSTWriter CExpr) -> CSTWriter CExpr
maybeParens p1 p2 mkCExpr = if p1 < p2
  then CEParen <$> (mkCExpr topPrec)
  else mkCExpr p2

makeVec2 :: CExpr -> CExpr -> CExpr
makeVec2 x y
  | x == y             = CEVec2Shorthand x
  | otherwise          = CEVec2 x y

makeVec3 :: CExpr -> CExpr -> CExpr -> CExpr
makeVec3 x y z
  | x == y && y == z   = CEVec3Shorthand x
  | otherwise          = CEVec3 x y z


type CSTWriter a = Writer CBlock a

-- Partial functions reference the fact that not all expressions in the grammar are well typed.
-- Should (knock on wood) still be a total functions over well typed expressions
fromExpr :: Partial => forall a. Expr a -> CBlock
fromExpr e = block <> [CSReturn expr]
  where
    (Tuple expr block) = runWriter $ fromExprTop e

fromExprTop :: Partial => forall a. Expr a -> CSTWriter CExpr
fromExprTop e = fromExprPrec topPrec e

fromExprPrec :: Partial => forall a. Int -> Expr a -> CSTWriter CExpr
fromExprPrec p (EVar name)    = pure $ CEVar name
fromExprPrec p (ENum n)       = pure $ CEFloat n
fromExprPrec p (EBool b)      = pure $ CEBool b
fromExprPrec p (EVec2 x y)    = makeVec2 <$> fromExprTop x <*> fromExprTop y
fromExprPrec p (EVec3 x y z)  = makeVec3 <$> fromExprTop x <*> fromExprTop y <*> fromExprTop z
fromExprPrec p (EComplex r i) = makeVec2 <$> fromExprTop r <*> fromExprTop i
fromExprPrec p (EColor r g b) = makeVec3 <$> fromExprTop r <*> fromExprTop g <*> fromExprTop b
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
    mkIf q = CEIf <$> fromExprPrec q i <*> fromExprPrec q t <*> fromExprPrec q e
fromExprPrec p (EBind v ty e1 e2) = do
  e1' <- fromExprTop e1
  tell $ [CSDecl (fromType ty) v e1']
  e2' <- fromExprTop e2
  pure e2'
fromExprPrec p (EBindRec n v ty e1 loop e2) = fromRecExpr n v ty e1 loop e2
-- No concrete representation for these types. Handle by elaborating
fromExprPrec p (ETuple _ _)       = crash
fromExprPrec p (EFst _)           = crash
fromExprPrec p (ESnd _)           = crash
fromExprPrec p (EInl _ )          = crash
fromExprPrec p (EInr _)           = crash
fromExprPrec p (EUnit)            = crash
fromExprPrec p (EMatch _ _ _ _ _) = crash

fromType :: Partial => Type -> String
fromType TBoolean = "bool"
fromType TScalar  = "float"
fromType TVec2    = "vec2"
fromType TVec3    = "vec3"
fromType TComplex = "vec2"
fromType TColor   = "vec3"
-- Handle by elaborating
fromType (TTuple _ _) = crash
fromType (TEither _ _) = crash

fromUnaryExpr :: Partial => forall a. Int -> UnaryExpr a -> CSTWriter CExpr
fromUnaryExpr p (UnNegate e)         = CEPrefix "-" <$> fromExprPrec p e
fromUnaryExpr p (UnNot e)            = CEPrefix "!" <$> fromExprPrec p e
fromUnaryExpr p (UnProjV2X e)        = CEPostfix ".x" <$> fromExprPrec p e
fromUnaryExpr p (UnProjV2Y e)        = CEPostfix ".y" <$> fromExprPrec p e
fromUnaryExpr p (UnProjV3X e)        = CEPostfix ".x" <$> fromExprPrec p e
fromUnaryExpr p (UnProjV3Y e)        = CEPostfix ".y" <$> fromExprPrec p e
fromUnaryExpr p (UnProjV3Z e)        = CEPostfix ".z" <$> fromExprPrec p e
fromUnaryExpr p (UnProjR e)          = CEPostfix ".r" <$> fromExprPrec p e
fromUnaryExpr p (UnProjG e)          = CEPostfix ".g" <$> fromExprPrec p e
fromUnaryExpr p (UnProjB e)          = CEPostfix ".b" <$> fromExprPrec p e
fromUnaryExpr p (UnProjReal e)       = CEPostfix ".x" <$> fromExprPrec p e
fromUnaryExpr p (UnProjImaginary e)  = CEPostfix ".y" <$> fromExprPrec p e

fromBinaryExpr :: Partial => forall a. Int -> BinaryExpr a -> CSTWriter CExpr
fromBinaryExpr p (BinEq e1 e2)        = CEBinary "==" <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinNeq e1 e2)       = CEBinary "!=" <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinAnd e1 e2)       = CEBinary "&&" <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinOr e1 e2)        = CEBinary "||" <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinLt e1 e2)        = CEBinary "<"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinLte e1 e2)       = CEBinary "<=" <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinGt e1 e2)        = CEBinary ">"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinGte e1 e2)       = CEBinary ">=" <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinPlus e1 e2)      = CEBinary "+"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinMinus e1 e2)     = CEBinary "-"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinTimes e1 e2)     = CEBinary "*"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinDiv e1 e2)       = CEBinary "/"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinPlusV2 e1 e2)    = CEBinary "+"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinMinusV2 e1 e2)   = CEBinary "-"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinScaleV2 e1 e2)   = CEBinary "*"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinPlusV3 e1 e2)    = CEBinary "+"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinMinusV3 e1 e2)   = CEBinary "-"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinScaleV3 e1 e2)   = CEBinary "*"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinPlusC e1 e2)     = CEBinary "+"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinMinusC e1 e2)    = CEBinary "-"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinScaleC e1 e2)    = CEBinary "*"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinPlusCol e1 e2)   = CEBinary "+"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinMinusCol e1 e2)  = CEBinary "-"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinTimesCol e1 e2)  = CEBinary "*"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
fromBinaryExpr p (BinScaleCol e1 e2)  = CEBinary "*"  <$> fromExprPrec p e1 <*> fromExprPrec p e2
-- Handle these ops by elaborating
fromBinaryExpr p (BinDivC _ _)        = crash
fromBinaryExpr p (BinTimesC _ _)      = crash

fromCallExpr :: Partial => forall a. CallExpr a -> CSTWriter CExpr
fromCallExpr (FnAbs e)               = CECall "abs"        <$> sequence [fromExprTop e]
fromCallExpr (FnCos e)               = CECall "cos"        <$> sequence [fromExprTop e]
fromCallExpr (FnFloor e)             = CECall "floor"      <$> sequence [fromExprTop e]
fromCallExpr (FnFract e)             = CECall "fract"      <$> sequence [fromExprTop e]
fromCallExpr (FnLog e)               = CECall "log"        <$> sequence [fromExprTop e]
fromCallExpr (FnLog2 e)              = CECall "log2"       <$> sequence [fromExprTop e]
fromCallExpr (FnSaturate e)          = CECall "saturate"   <$> sequence [fromExprTop e]
fromCallExpr (FnSin e)               = CECall "sin"        <$> sequence [fromExprTop e]
fromCallExpr (FnSqrt e)              = CECall "sqrt"       <$> sequence [fromExprTop e]
fromCallExpr (FnAtan e1 e2)          = CECall "atan"       <$> sequence [fromExprTop e1, fromExprTop e2]
fromCallExpr (FnMax e1 e2)           = CECall "max"        <$> sequence [fromExprTop e1, fromExprTop e2]
fromCallExpr (FnMin e1 e2)           = CECall "min"        <$> sequence [fromExprTop e1, fromExprTop e2]
fromCallExpr (FnMod e1 e2)           = CECall "mod"        <$> sequence [fromExprTop e1, fromExprTop e2]
fromCallExpr (FnPow e1 e2)           = CECall "pow"        <$> sequence [fromExprTop e1, fromExprTop e2]
fromCallExpr (FnSmoothstep e1 e2 e3) = CECall "smoothstep" <$> sequence [fromExprTop e1, fromExprTop e2, fromExprTop e3]
fromCallExpr (FnLengthV2 e)          = CECall "length"     <$> sequence [fromExprTop e]
fromCallExpr (FnLengthV3 e)          = CECall "length"     <$> sequence [fromExprTop e]
fromCallExpr (FnNormalizeV2 e)       = CECall "normalize"  <$> sequence [fromExprTop e]
fromCallExpr (FnNormalizeV3 e)       = CECall "normalize"  <$> sequence [fromExprTop e]
fromCallExpr (FnDotV2 e1 e2)         = CECall "dot"        <$> sequence [fromExprTop e1, fromExprTop e2]
fromCallExpr (FnDotV3 e1 e2)         = CECall "dot"        <$> sequence [fromExprTop e1, fromExprTop e2]
fromCallExpr (FnDotC e1 e2)          = CECall "dot"        <$> sequence [fromExprTop e1, fromExprTop e2]
fromCallExpr (FnReflectV2 e1 e2)     = CECall "reflect"    <$> sequence [fromExprTop e1, fromExprTop e2]
fromCallExpr (FnReflectV3 e1 e2)     = CECall "reflect"    <$> sequence [fromExprTop e1, fromExprTop e2]

-- TODO: I don't like how complicated this is
fromRecExpr :: Partial => forall a b c. Int -> String -> Type -> Expr a -> Expr b -> Expr c -> CSTWriter CExpr
fromRecExpr n v ty e1 loop e2 = do
  _ <- fromExprTop $ normalize e1'
  tell $ [CSLoop n $ fromLoopBody v (removeFakes $ normalize loop')]
  fromExprTop $ removeFakes $ normalize e2'
  where
    e1'   = EBind v ty (eraseType e1) (EVar "fake")
    loop' = EBind v ty (EVar "fake")                -- Dummy declaration so that elaboration kicks in.
      $ EBind v ty (fromIso $ eraseType loop)       -- Actual loop update
      $ eitherToBool (resultExpr $ eraseType loop)  -- Loop break statement
    e2'   = EBind v ty (EVar "fake")                -- Dummy declaration so that elaboration kicks in.
      $ e2                                          -- bound expression after the loop
    eraseType :: forall a b. Expr a -> Expr b
    eraseType = unsafeCoerce
    removeFakes :: forall a. Expr a -> Expr a
    removeFakes e@(EBind _ _ ex1 ex2)
      | hasVar "fake" ex1 = removeFakes ex2
      | otherwise         = e
    removeFakes e = e

resultExpr :: forall a. Expr a -> Expr a
resultExpr (EBind _ _ _ e2) = resultExpr e2
resultExpr e = e

hasVar :: forall a. String -> Expr a -> Boolean
hasVar v e = unwrap $ foldVars (Disj <$> (_ == v)) e

fromLoopBody :: Partial => String -> Expr Boolean -> CBlock
fromLoopBody v e = (convertDecl v <$> block) <> [CSIf expr [CSBreak] Nothing]
  where
    (Tuple expr block) = runWriter $ fromExprTop e

eitherToBool :: forall a b. Expr (Either a b) -> Expr Boolean
eitherToBool e = matchE e "" (const ff) (const tt)

fromIso :: forall a. Expr (Either a a) -> Expr a
fromIso e = matchE e "" identity identity

convertDecl :: String -> CStmt -> CStmt
convertDecl v (CSDecl ty n e)
  | v == n                     = CSAssign n e
  | startsWith (v <> "_") n    = CSAssign n e
  | otherwise                  = CSDecl ty n e
convertDecl v (CSAssign n e)   = CSAssign n e
convertDecl v (CSReturn e)     = CSReturn e
convertDecl v (CSIf i thn els) = CSIf i (convertDecl v <$> thn) ((liftA1 $ convertDecl v) <$> els)
convertDecl v (CSLoop n block) = CSLoop n (convertDecl v <$> block)
convertDecl v (CSBreak)        = CSBreak
