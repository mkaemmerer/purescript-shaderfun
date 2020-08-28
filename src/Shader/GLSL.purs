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
  printType ty <> " " <> name <> " = " <> printExpr' val <> ";\n" <> printExprWithReturn body
printExprWithReturn e =
  "return " <> printExpr' e <> ";"

printExpr' :: Partial => forall a. Expr a -> String
printExpr' (EVar name)              = name
printExpr' (ENum n)                 = show n
printExpr' (EBool b)                = show b
printExpr' (EVec2 x y)              = "vec2(" <> printList (printExpr' <$> [ x, y ]) <> ")"
printExpr' (EVec3 x y z)            = "vec3(" <> printList (printExpr' <$> [ x, y, z ]) <> ")"
printExpr' (EComplex r i)           = "vec2(" <> printList (printExpr' <$> [ r, i]) <> ")"
printExpr' (EColor r g b)           = "vec3(" <> printList (printExpr' <$> [ r, g, b ]) <> ")"
printExpr' (EUnary op e)            = printUnary op e
printExpr' (EBinary op l r)         = printBinary op l r
printExpr' (EParen e)               = "(" <> printExpr' e <> ")"
printExpr' (ECall fn args)          = fn <> "(" <> printList (printExpr' <$> args) <> ")"
printExpr' (EIf i t e)              = printExpr' i <> " ? " <> printExpr' t <> " : " <> printExpr' e
-- When unit is used as a sentinel value, don't initialize to anything
printExpr' (EBind name ty EUnit body) = printType ty <> " " <> name <> ";\n" <> printExpr' body
printExpr' (EBind name ty val body)   = printType ty <> " " <> name <> " = " <> printExpr' val <> ";\n" <> printExpr' body
-- Not handled
printExpr' (ETuple _ _) = crash
printExpr' (EFst _) = crash
printExpr' (ESnd _) = crash
printExpr' (EUnit) = crash

printType :: Type -> String
printType TBoolean = "bool"
printType TScalar  = "float"
printType TVec2    = "vec2"
printType TVec3    = "vec3"
printType TComplex = "vec2"
printType TColor   = "vec3"

printUnary :: Partial => forall a. UnaryOp -> Expr a -> String
printUnary OpNegate e = "-" <> printExpr' e
printUnary OpNot    e = "!" <> printExpr' e
printUnary OpProjX  e = printExpr' e <> ".x"
printUnary OpProjY  e = printExpr' e <> ".y"
printUnary OpProjR  e = printExpr' e <> ".r"
printUnary OpProjG  e = printExpr' e <> ".g"
printUnary OpProjB  e = printExpr' e <> ".b"
printUnary OpProjReal       e = printExpr' e <> ".x"
printUnary OpProjImaginary  e = printExpr' e <> ".y"

printBinary :: Partial => forall a. BinaryOp -> Expr a -> Expr a -> String
printBinary op l r = intercalate " " [ printExpr' l, printBinaryOp op, printExpr' r ]
  where
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

-- Normalization 
normalize :: forall a. TypedExpr a => Expr a -> Expr a
normalize = unsafePartial $ elaborate >>> liftDecl >>> fixPrecedence

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
elaborate (EParen e)               = EParen (elaborate e)
elaborate (ECall fn args)          = ECall fn (elaborate <$> args)
elaborate (EIf i t e)              = EIf (elaborate i) (elaborate t) (elaborate e)
elaborate (EBind name ty val body) = EBind name ty (elaborate val) (elaborate body)
-- Not handled
elaborate (EFst _) = crash     -- ETuple/EIf are the only inhabitants of type (Expr (Tuple a b))
elaborate (ESnd _) = crash     -- ETuple/EIf are the only inhabitants of type (Expr (Tuple a b))
elaborate (ETuple _ _) = crash -- ETuple is not a valid top-level term
elaborate (EUnit) = crash      -- Unit is not a valid top-level term

topPrec :: Int
topPrec = 100

unaryPrecedence :: UnaryOp -> Int
unaryPrecedence OpNegate = 1
unaryPrecedence OpNot    = 1
unaryPrecedence OpProjX  = 2
unaryPrecedence OpProjY  = 2
unaryPrecedence OpProjZ  = 2
unaryPrecedence OpProjR  = 2
unaryPrecedence OpProjG  = 2
unaryPrecedence OpProjB  = 2
unaryPrecedence OpProjReal      = 2
unaryPrecedence OpProjImaginary = 2

binaryPrecedence :: BinaryOp -> Int
binaryPrecedence OpTimes     = 3
binaryPrecedence OpDiv       = 3
binaryPrecedence OpScaleV    = 4
binaryPrecedence OpScaleC    = 4
binaryPrecedence OpTimesC    = 4
binaryPrecedence OpDivC      = 4
binaryPrecedence OpScaleCol  = 4
binaryPrecedence OpTimesCol  = 4
binaryPrecedence OpPlus      = 5
binaryPrecedence OpPlusV     = 6
binaryPrecedence OpPlusC     = 6
binaryPrecedence OpPlusCol   = 6
binaryPrecedence OpMinus     = 5
binaryPrecedence OpMinusV    = 6
binaryPrecedence OpMinusC    = 6
binaryPrecedence OpMinusCol  = 6
binaryPrecedence OpLt        = 7
binaryPrecedence OpLte       = 7
binaryPrecedence OpGt        = 7
binaryPrecedence OpGte       = 7
binaryPrecedence OpEq        = 8
binaryPrecedence OpNeq       = 8
binaryPrecedence OpAnd       = 9
binaryPrecedence OpOr        = 10

-- Add parenthesis to an expression to preserve its meaning
-- Ideally, only add parenthesis when needed
fixPrecedence :: Partial => forall a. Expr a -> Expr a
fixPrecedence e = fixPrecedence' topPrec e

fixPrecedence' :: Partial => forall a. Int -> Expr a -> Expr a
fixPrecedence' prec (EVar name) = EVar name
fixPrecedence' prec (EBool b) = EBool b
fixPrecedence' prec (ENum n) = ENum n
fixPrecedence' prec (EVec2 x y) = EVec2 x' y'
  where
    x' = fixPrecedence' topPrec x
    y' = fixPrecedence' topPrec y
fixPrecedence' prec (EVec3 x y z) = EVec3 x' y' z'
  where
    x' = fixPrecedence' topPrec x
    y' = fixPrecedence' topPrec y
    z' = fixPrecedence' topPrec z
fixPrecedence' prec (EComplex r i) = EComplex r' i'
  where
    r' = fixPrecedence' topPrec r
    i' = fixPrecedence' topPrec i
fixPrecedence' prec (EColor r g b) = EColor r' g' b'
  where
    r' = fixPrecedence' topPrec r
    g' = fixPrecedence' topPrec g
    b' = fixPrecedence' topPrec b
fixPrecedence' prec (EUnary op e) = if opPrec < prec then inner else EParen (inner)
  where
    opPrec = unaryPrecedence op
    inner = EUnary op (fixPrecedence' opPrec e)
fixPrecedence' prec (EBinary op l r) = if opPrec < prec then inner else EParen (inner)
  where
    opPrec = binaryPrecedence op
    inner = EBinary op (fixPrecedence' opPrec l) (fixPrecedence' opPrec r)
fixPrecedence' prec (EParen e) = EParen (fixPrecedence' topPrec e)
fixPrecedence' prec (ECall fn args) = ECall fn (fixPrecedence' topPrec <$> args)
fixPrecedence' prec (EIf i t e) = if ifPrec < prec then inner else EParen (inner)
  where
    ifPrec = 11
    inner = EIf (fixPrecedence' ifPrec i) (fixPrecedence' ifPrec t) (fixPrecedence' ifPrec e)
fixPrecedence' prec (EBind name ty val body) = EBind name ty (fixPrecedence' topPrec val) body'
  where
    body' = fixPrecedence' topPrec body
-- Not handled
fixPrecedence' prec (ETuple _ _) = crash
fixPrecedence' prec (EFst _) = crash
fixPrecedence' prec (ESnd _) = crash
fixPrecedence' prec (EUnit) = crash


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
liftDecl' (EParen e) = do
  e' <- liftDecl' e
  callCC $ (#) $ EParen e'
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
