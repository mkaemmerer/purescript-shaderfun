module Shader (ShaderExpr, Vec2, Color, num, vec2, color) where

import Prelude

import Data.Foldable (intercalate)
import Data.Leibniz (type (~))

data Vec2 = Vec2 Number Number
data Color = Color Number Number Number

data UnaryOp
  = OpNegate
  | OpNot
  | OpProjX
  | OpProjY
  | OpProjR
  | OpProjG
  | OpProjB

data BinaryOp
  = OpEq
  | OpNeq
  | OpAnd
  | OpOr
  | OpPlus
  | OpMinus
  | OpTimes
  | OpDiv
  | OpLt
  | OpLte
  | OpGt
  | OpGte
  | OpVPlus
  | OpVMinus
  | OpScale

data ShaderExpr t
  = EVar String
  | ENum Number (t ~ Number)
  | EBool Boolean (t ~ Boolean)
  | EVec2 (ShaderExpr Number) (ShaderExpr Number) (t ~ Vec2)
  | EColor (ShaderExpr Number) (ShaderExpr Number) (ShaderExpr Number) (t ~ Color)
  | EUnary UnaryOp (ShaderExpr t)
  | EBinary BinaryOp (ShaderExpr t) (ShaderExpr t)
  | EParen (ShaderExpr t)
  -- | EIf (ShaderExpr Boolean) (ShaderExpr t) (ShaderExpr t)
  -- | EBind 
  -- | ECall


-- Show instances
instance showUnaryOp :: Show UnaryOp where
  show OpNegate = "-"
  show OpNot    = "!"
  show OpProjX  = ".x"
  show OpProjY  = ".y"
  show OpProjR  = ".r"
  show OpProjG  = ".g"
  show OpProjB  = ".b"

instance showBinaryOp :: Show BinaryOp where
  show OpEq     = "=="
  show OpNeq    = "!="
  show OpAnd    = "&&"
  show OpOr     = "||"
  show OpPlus   = "+"
  show OpMinus  = "-"
  show OpTimes  = "*"
  show OpDiv    = "/"
  show OpLt     = "<"
  show OpLte    = "<="
  show OpGt     = ">"
  show OpGte    = ">="
  show OpVPlus  = "+"
  show OpVMinus = "-"
  show OpScale  = "*"

instance showShaderExpr :: Show (ShaderExpr a) where
  show (EVar name)           = name
  show (ENum n proof)        = show n
  show (EBool b proof)       = show b
  show (EVec2 x y proof)     = "vec2(" <> showList [x, y] <> ")"
  show (EColor r g b proof)  = "vec3(" <> showList [r,g,b] <> ")"
  show (EUnary op e)         = showUnary op e
  show (EBinary op l r)      = showBinary op l r
  show (EParen e)            = "(" <> show e <> ")"

showList :: forall a. (Show a) => Array a -> String
showList ss = intercalate ", " (map show ss)

showUnary :: forall a. UnaryOp -> ShaderExpr a -> String
showUnary OpNegate e = "-" <> show e
showUnary OpNot    e = "!" <> show e
showUnary OpProjX  e = show e <> ".x"
showUnary OpProjY  e = show e <> ".y"
showUnary OpProjR  e = show e <> ".r"
showUnary OpProjG  e = show e <> ".g"
showUnary OpProjB  e = show e <> ".b"

showBinary :: forall a. BinaryOp -> ShaderExpr a -> ShaderExpr a -> String
showBinary op l r = intercalate " " [show l, show op, show r]


-- Smart constructors
num :: Number -> ShaderExpr Number
num n = ENum n identity

vec2 :: ShaderExpr Number -> ShaderExpr Number -> ShaderExpr Vec2
vec2 x y = EVec2 x y identity

color :: ShaderExpr Number -> ShaderExpr Number -> ShaderExpr Number -> ShaderExpr Color
color r g b = EColor r g b identity

paren :: forall a. ShaderExpr a -> ShaderExpr a
paren e = EParen e

instance semiringShaderExprNumber :: Semiring (ShaderExpr Number) where
  zero = ENum 0.0 identity
  one = ENum 1.0 identity
  add e1 e2 = EBinary OpPlus e1 e2
  mul e1 e2 = EBinary OpTimes e1 e2

instance ringShaderExprNumber :: Ring (ShaderExpr Number) where
  sub e1 e2 = EBinary OpMinus e1 e2

instance divisionRingShaderExprNumber :: DivisionRing (ShaderExpr Number) where
  recip e = EBinary OpDiv (num 1.0) e

instance commutativeRingShaderExprNumber :: CommutativeRing (ShaderExpr Number)

instance euclideanRingShaderExprNumber :: EuclideanRing (ShaderExpr Number) where
  div e1 e2 = EBinary OpDiv e1 e2
  degree _ = 1
  mod _ _ = num 0.0 -- A proper lawful definition, but not a useful one for shader programming