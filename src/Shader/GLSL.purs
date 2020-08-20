module Shader.GLSL (toGLSL) where

import Prelude

import Control.Monad.Cont (Cont, runCont, callCC)
import Data.Foldable (intercalate)
import Data.Traversable (sequence)
import Shader.Expr (BinaryOp(..), Expr(..), Type(..), UnaryOp(..))
import Unsafe.Coerce (unsafeCoerce)

-- Code generation
toGLSL :: forall a. Expr a -> String
toGLSL = normalize >>> printExpr

printList :: Array String -> String
printList = intercalate ", "

printExpr :: forall a. Expr a -> String
printExpr = printExprWithReturn

printExprWithReturn :: forall a. Expr a -> String
printExprWithReturn (EBind name ty val body) =
  printType ty <> " " <> name <> " = " <> printExpr' val <> ";\n" <> printExprWithReturn body
printExprWithReturn e =
  "return " <> printExpr' e <> ";"

printExpr' :: forall a. Expr a -> String
printExpr' (EVar name)              = name
printExpr' (ENum n)                 = show n
printExpr' (EBool b)                = show b
printExpr' (EVec2 x y)              = "vec2(" <> printList [ printExpr' x, printExpr' y ] <> ")"
printExpr' (EColor r g b)           = "vec3(" <> printList [ printExpr' r, printExpr' g, printExpr' b ] <> ")"
printExpr' (EUnary op e)            = printUnary op e
printExpr' (EBinary op l r)         = printBinary op l r
printExpr' (EParen e)               = "(" <> printExpr' e <> ")"
printExpr' (ECall fn args)          = fn <> "(" <> printList (printExpr' <$> args) <> ")"
printExpr' (EIf i t e)              = printExpr' i <> " ? " <> printExpr' t <> " : " <> printExpr' e
printExpr' (EBind name ty val body) = printType ty <> " " <> name <> " = " <> printExpr' val <> ";\n" <> printExpr' body

printType :: Type -> String
printType TBoolean = "bool"
printType TScalar  = "float"
printType TVec2    = "vec2"
printType TColor   = "vec3"

printUnary :: forall a. UnaryOp -> Expr a -> String
printUnary OpNegate e = "-" <> printExpr' e
printUnary OpNot    e = "!" <> printExpr' e
printUnary OpProjX  e = printExpr' e <> ".x"
printUnary OpProjY  e = printExpr' e <> ".y"
printUnary OpProjR  e = printExpr' e <> ".r"
printUnary OpProjG  e = printExpr' e <> ".g"
printUnary OpProjB  e = printExpr' e <> ".b"

printBinary :: forall a. BinaryOp -> Expr a -> Expr a -> String
printBinary op l r = intercalate " " [ printExpr' l, printBinaryOp op, printExpr' r ]
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
normalize :: forall a. Expr a -> Expr a
normalize = liftDecl >>> fixPrecedence

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

fixPrecedence :: forall a. Expr a -> Expr a
fixPrecedence e = fixPrecedence' topPrec e

fixPrecedence' :: forall a. Int -> Expr a -> Expr a
fixPrecedence' prec (EVar name) = EVar name
fixPrecedence' prec (ENum n) = ENum n
fixPrecedence' prec (EBool b) = EBool b
fixPrecedence' prec (EVec2 x y) = EVec2 x' y'
  where
    x' = fixPrecedence' topPrec x
    y' = fixPrecedence' topPrec y
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


id :: forall a. a -> a
id x = x

eraseType :: forall a b. Expr a -> Expr b
eraseType = unsafeCoerce

liftDecl :: forall a. Expr a -> Expr a
liftDecl e = runCont (liftDecl' e) id

liftDecl' :: forall a b. Expr a -> Cont (Expr b) (Expr a)
liftDecl' (EVar name) = callCC $ (#) $ EVar name
liftDecl' (ENum n) = callCC $ (#) $ ENum n
liftDecl' (EBool b) = callCC $ (#) $ EBool b
liftDecl' (EVec2 x y) = do
  x' <- liftDecl' x
  y' <- liftDecl' y
  callCC $ (#) $ EVec2 x' y'
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
