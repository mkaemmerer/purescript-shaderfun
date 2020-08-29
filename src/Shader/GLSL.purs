module Shader.GLSL (toGLSL) where

import Prelude

import Control.Monad.Cont (Cont, runCont, callCC)
import Data.Foldable (intercalate)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Partial (crash)
import Partial.Unsafe (unsafePartial)
import Shader.Expr (class TypedExpr, BinaryOp(..), Expr(..), Type(..), UnaryOp(..), complex, projImaginary, projReal)
import Unsafe.Coerce (unsafeCoerce)

-- | Convert an expression to GLSL code
toGLSL :: forall a. TypedExpr a => Expr a -> String
toGLSL = unsafePartial $ normalize >>> printExpr

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
printExpr' p (EUnary op e)            = printUnary p op e
printExpr' p (EBinary op l r)         = printBinary p op l r
printExpr' p (ECall fn args)          = fn <> "(" <> printList (printExpr' topPrec <$> args) <> ")"
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

printUnary :: Partial => forall a. Int -> UnaryOp -> Expr a -> String
printUnary p op e = if p < opPrec
    then printUnaryOp op $ "(" <> printExpr' topPrec e <> ")"
    else printUnaryOp op $ printExpr' opPrec e
  where
    opPrec = unaryPrec op
    printUnaryOp OpNegate         str = "-" <> str
    printUnaryOp OpNot            str = "!" <> str
    printUnaryOp OpProjX          str = str <> ".x"
    printUnaryOp OpProjY          str = str <> ".y"
    printUnaryOp OpProjR          str = str <> ".r"
    printUnaryOp OpProjG          str = str <> ".g"
    printUnaryOp OpProjB          str = str <> ".b"
    printUnaryOp OpProjReal       str = str <> ".x"
    printUnaryOp OpProjImaginary  str = str <> ".y"

printBinary :: Partial => forall a. Int -> BinaryOp -> Expr a -> Expr a -> String
printBinary p op l r = if p < opPrec
    then "(" <> printBinary' topPrec <> ")"
    else printBinary' opPrec
  where
    printBinary' prec = intercalate " " [ printExpr' prec l, printBinaryOp op, printExpr' prec r ]
    opPrec = binaryPrec op
    printBinaryOp OpEq        = "=="
    printBinaryOp OpNeq       = "!="
    printBinaryOp OpAnd       = "&&"
    printBinaryOp OpOr        = "||"
    printBinaryOp OpLt        = "<"
    printBinaryOp OpLte       = "<="
    printBinaryOp OpGt        = ">"
    printBinaryOp OpGte       = ">="
    printBinaryOp OpPlus      = "+"
    printBinaryOp OpMinus     = "-"
    printBinaryOp OpTimes     = "*"
    printBinaryOp OpDiv       = "/"
    printBinaryOp OpPlusV     = "+"
    printBinaryOp OpMinusV    = "-"
    printBinaryOp OpScaleV    = "*"
    printBinaryOp OpPlusC     = "+"
    printBinaryOp OpMinusC    = "-"
    printBinaryOp OpScaleC    = "*"
    printBinaryOp OpPlusCol   = "+"
    printBinaryOp OpMinusCol  = "-"
    printBinaryOp OpTimesCol  = "*"
    printBinaryOp OpScaleCol  = "*"
    printBinaryOp OpDivC   = crash -- GLSL has no builtin operation. Handled by "elaborating"
    printBinaryOp OpTimesC = crash -- GLSL has no builtin operation. Handled by "elaborating"

topPrec :: Int
topPrec = 100

unaryPrec :: UnaryOp -> Int
unaryPrec OpNegate = 1
unaryPrec OpNot    = 1
unaryPrec OpProjX  = 2
unaryPrec OpProjY  = 2
unaryPrec OpProjZ  = 2
unaryPrec OpProjR  = 2
unaryPrec OpProjG  = 2
unaryPrec OpProjB  = 2
unaryPrec OpProjReal      = 2
unaryPrec OpProjImaginary = 2

binaryPrec :: BinaryOp -> Int
binaryPrec OpTimes     = 3
binaryPrec OpDiv       = 3
binaryPrec OpScaleV    = 4
binaryPrec OpScaleC    = 4
binaryPrec OpTimesC    = 4
binaryPrec OpDivC      = 4
binaryPrec OpScaleCol  = 4
binaryPrec OpTimesCol  = 4
binaryPrec OpPlus      = 5
binaryPrec OpPlusV     = 6
binaryPrec OpPlusC     = 6
binaryPrec OpPlusCol   = 6
binaryPrec OpMinus     = 5
binaryPrec OpMinusV    = 6
binaryPrec OpMinusC    = 6
binaryPrec OpMinusCol  = 6
binaryPrec OpLt        = 7
binaryPrec OpLte       = 7
binaryPrec OpGt        = 7
binaryPrec OpGte       = 7
binaryPrec OpEq        = 8
binaryPrec OpNeq       = 8
binaryPrec OpAnd       = 9
binaryPrec OpOr        = 10

ifPrec :: Int
ifPrec = 11

-- Normalization 
normalize :: forall a. TypedExpr a => Expr a -> Expr a
normalize = unsafePartial $ elaborate >>> liftDecl

elaborate :: Partial => forall a. Expr a -> Expr a
elaborate (EBinary OpTimesC c1 c2) = eraseType $ complex (r1*r2 - i1*i2) (r1*i2 + r2*i1)
  where
  c1' = elaborate c1
  c2' = elaborate c2
  Tuple r1 i1 = Tuple (projReal c1') (projImaginary c1')
  Tuple r2 i2 = Tuple (projReal c2') (projImaginary c2')
elaborate (EBinary OpDivC c1 c2) = eraseType $ complex nr ni
  where
  c1' = elaborate c1
  c2' = elaborate c2
  Tuple r1 i1 = Tuple (projReal c1') (projImaginary c1')
  Tuple r2 i2 = Tuple (projReal c2') (projImaginary c2')
  nr = (r1*r2 + i1*i2) / d
  ni = (r2*i1 - r1*i2) / d
  d  = r2*r2 + i2*i2
elaborate (EFst (ETuple e1 e2)) = elaborate e1
elaborate (ESnd (ETuple e1 e2)) = elaborate e2
elaborate (EFst (EIf c t e)) = EIf (elaborate c) (elaborate $ EFst t) (elaborate $ EFst e)
elaborate (ESnd (EIf c t e)) = EIf (elaborate c) (elaborate $ ESnd t) (elaborate $ ESnd e)
-- Other cases
elaborate (EVar name)              = EVar name
elaborate (ENum n)                 = ENum n
elaborate (EBool b)                = EBool b
elaborate (EVec2 x y)              = EVec2 (elaborate x) (elaborate y)
elaborate (EVec3 x y z)            = EVec3 (elaborate x) (elaborate y) (elaborate z)
elaborate (EComplex r i)           = EComplex (elaborate r) (elaborate i)
elaborate (EColor r g b)           = EColor (elaborate r) (elaborate g) (elaborate b)
elaborate (EUnary op e)            = EUnary op (elaborate e)
elaborate (EBinary op l r)         = EBinary op (elaborate l) (elaborate r)
elaborate (ECall fn args)          = ECall fn (elaborate <$> args)
elaborate (EIf i t e)              = EIf (elaborate i) (elaborate t) (elaborate e)
elaborate (EBind name ty val body) = EBind name ty (elaborate val) (elaborate body)
-- Not handled
elaborate (EFst _) = crash     -- ETuple/EIf are the only inhabitants of type (Expr (Tuple a b))
elaborate (ESnd _) = crash     -- ETuple/EIf are the only inhabitants of type (Expr (Tuple a b))
elaborate (ETuple _ _) = crash -- ETuple is not a valid top-level term
elaborate (EUnit) = crash      -- Unit is not a valid top-level term


eraseType :: forall a b. Expr a -> Expr b
eraseType = unsafeCoerce

-- Hoist declarations to top level
liftDecl :: Partial => forall a. Expr a -> Expr a
liftDecl e = runCont (liftDecl' e) identity

liftDecl' :: Partial => forall a b. Expr a -> Cont (Expr b) (Expr a)
liftDecl' (EVar name) = callCC $ (#) $ EVar name
liftDecl' (EBool b) = callCC $ (#) $ EBool b
liftDecl' (ENum n) = callCC $ (#) $ ENum n
liftDecl' (EVec2 x y) = do
  x' <- liftDecl' x
  y' <- liftDecl' y
  callCC $ (#) $ EVec2 x' y'
liftDecl' (EVec3 x y z) = do
  x' <- liftDecl' x
  y' <- liftDecl' y
  z' <- liftDecl' z
  callCC $ (#) $ EVec3 x' y' z'
liftDecl' (EComplex r i) = do
  r' <- liftDecl' r
  i' <- liftDecl' i
  callCC $ (#) $ EComplex r' i'
liftDecl' (EColor r g b) = do
  r' <- liftDecl' r
  g' <- liftDecl' g
  b' <- liftDecl' b
  callCC $ (#) $ EColor r' g' b'
liftDecl' (EUnary op e) = do
  e' <- liftDecl' e
  callCC $ (#) $ EUnary op (eraseType e')
liftDecl' (EBinary op l r) = do
  l' <- liftDecl' l
  r' <- liftDecl' r
  callCC $ (#) $ EBinary op (eraseType l') (eraseType r')
liftDecl' (ECall fn args) = do
  args' <- sequence $ liftDecl' <$> args
  callCC $ (#) $ ECall fn (eraseType <$> args')
liftDecl' (EIf i t e) = do
  i' <- liftDecl' i
  t' <- liftDecl' t
  e' <- liftDecl' e
  callCC $ (#) $ EIf i' t' e'
liftDecl' (EBind name ty val body) = do
  body' <- liftDecl' body
  pure $ EBind name ty val body'
-- Not handled
liftDecl' (ETuple _ _) = crash
liftDecl' (EFst _) = crash
liftDecl' (ESnd _) = crash
liftDecl' (EUnit) = crash
