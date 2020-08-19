module Shader
  ( ShaderExpr
  , Vec2
  , Color
  , p
  , num
  , vec2
  , color
  , projX
  , projY
  , length
  , abs
  , floor
  , fract
  , smoothstep
  , sin
  , cos
  , log
  , log2
  , saturate
  , sqrt
  , atan
  , max
  , min
  , mod
  , absV
  , dot
  , mix
  ) where

import Prelude
import Data.Foldable (intercalate)
import Data.Leibniz (type (~))
import Unsafe.Coerce (unsafeCoerce)

data Vec2
  = Vec2 Number Number

data Color
  = Color Number Number Number

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
  | EUnary UnaryOp ShaderExpr'
  | EBinary BinaryOp ShaderExpr' ShaderExpr'
  | EParen (ShaderExpr t)
  | ECall String ShaderExprList'
-- | EIf (ShaderExpr Boolean) (ShaderExpr t) (ShaderExpr t)
-- | EBind

type ShaderExpr'
  = forall a. ShaderExpr a

type ShaderExprList'
  = forall a. Array (ShaderExpr a)


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
  show (EVar name)          = name
  show (ENum n proof)       = show n
  show (EBool b proof)      = show b
  show (EVec2 x y proof)    = "vec2(" <> showList [ x, y ] <> ")"
  show (EColor r g b proof) = "vec3(" <> showList [ r, g, b ] <> ")"
  show (EUnary op e)        = showUnary op e
  show (EBinary op l r)     = showBinary op l r
  show (EParen e)           = "(" <> show e <> ")"
  show (ECall fn args)      = fn <> "(" <> showList args <> ")"

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
showBinary op l r = intercalate " " [ show l, show op, show r ]


-- Unsafe, private constructors
unary :: forall a. forall b. UnaryOp -> ShaderExpr a -> ShaderExpr b
unary op e = EUnary op (unsafeCoerce e)

binary :: forall a. forall b. forall c. BinaryOp -> ShaderExpr a -> ShaderExpr b -> ShaderExpr c
binary op l r = EBinary op (unsafeCoerce l) (unsafeCoerce r)

call :: forall a. String -> ShaderExprList' -> ShaderExpr a
call fn args = ECall fn args


-- Smart constructors
num :: Number -> ShaderExpr Number
num n = ENum n identity

vec2 :: ShaderExpr Number -> ShaderExpr Number -> ShaderExpr Vec2
vec2 x y = EVec2 x y identity

color :: ShaderExpr Number -> ShaderExpr Number -> ShaderExpr Number -> ShaderExpr Color
color r g b = EColor r g b identity

paren :: forall a. ShaderExpr a -> ShaderExpr a
paren e = EParen e


-- The input point. A variable that we have "blessed" as a vector
p :: ShaderExpr Vec2
p = EVar "p"

projX :: ShaderExpr Vec2 -> ShaderExpr Number
projX v = unary OpProjX v

projY :: ShaderExpr Vec2 -> ShaderExpr Number
projY v = unary OpProjY v

projR :: ShaderExpr Color -> ShaderExpr Number
projR c = unary OpProjR c

projG :: ShaderExpr Color -> ShaderExpr Number
projG c = unary OpProjG c

projB :: ShaderExpr Color -> ShaderExpr Number
projB c = unary OpProjB c


-- Built-in functions
-- Scalar functions
abs :: ShaderExpr Number -> ShaderExpr Number
abs n = call "abs" [ unsafeCoerce n ]

cos :: ShaderExpr Number -> ShaderExpr Number
cos n = call "sin" [ unsafeCoerce n ]

floor :: ShaderExpr Number -> ShaderExpr Number
floor n = call "floor" [ unsafeCoerce n ]

fract :: ShaderExpr Number -> ShaderExpr Number
fract n = call "fract" [ unsafeCoerce n ]

log :: ShaderExpr Number -> ShaderExpr Number
log n = call "log" [ unsafeCoerce n ]

log2 :: ShaderExpr Number -> ShaderExpr Number
log2 n = call "log2" [ unsafeCoerce n ]

saturate :: ShaderExpr Number -> ShaderExpr Number
saturate n = call "saturate" [ unsafeCoerce n ]

sin :: ShaderExpr Number -> ShaderExpr Number
sin n = call "sin" [ unsafeCoerce n ]

sqrt :: ShaderExpr Number -> ShaderExpr Number
sqrt n = call "sqrt" [ unsafeCoerce n ]

atan :: ShaderExpr Number -> ShaderExpr Number -> ShaderExpr Number
atan y x = call "atan2" [ unsafeCoerce y, unsafeCoerce x ]

max :: ShaderExpr Number -> ShaderExpr Number -> ShaderExpr Number
max x y = call "max" [ unsafeCoerce x, unsafeCoerce y ]

min :: ShaderExpr Number -> ShaderExpr Number -> ShaderExpr Number
min x y = call "min" [ unsafeCoerce x, unsafeCoerce y ]

mod :: ShaderExpr Number -> ShaderExpr Number -> ShaderExpr Number
mod x y = call "mod" [ unsafeCoerce x, unsafeCoerce y ]

smoothstep :: ShaderExpr Number -> ShaderExpr Number -> ShaderExpr Number -> ShaderExpr Number
smoothstep lo hi x = call "smoothstep" [ unsafeCoerce lo, unsafeCoerce hi, unsafeCoerce x ]

-- Vector functions
absV :: ShaderExpr Vec2 -> ShaderExpr Vec2
absV v = call "abs" [ unsafeCoerce v ]

length :: ShaderExpr Vec2 -> ShaderExpr Number
length v = call "length" [ unsafeCoerce v ]

dot :: ShaderExpr Vec2 -> ShaderExpr Vec2 -> ShaderExpr Number
dot u v = call "dot" [ unsafeCoerce u, unsafeCoerce v ]

-- Color functions
mix :: ShaderExpr Color -> ShaderExpr Color -> ShaderExpr Number -> ShaderExpr Color
mix c1 c2 fac = call "mix" [ unsafeCoerce c1, unsafeCoerce c2, unsafeCoerce fac ]


-- Useful Instances
instance semiringShaderExprNumber :: Semiring (ShaderExpr Number) where
  zero = ENum 0.0 identity
  one = ENum 1.0 identity
  add e1 e2 = binary OpPlus e1 e2
  mul e1 e2 = binary OpTimes e1 e2

instance ringShaderExprNumber :: Ring (ShaderExpr Number) where
  sub e1 e2 = binary OpMinus e1 e2

instance divisionRingShaderExprNumber :: DivisionRing (ShaderExpr Number) where
  recip e = binary OpDiv (num 1.0) e

instance commutativeRingShaderExprNumber :: CommutativeRing (ShaderExpr Number)

instance euclideanRingShaderExprNumber :: EuclideanRing (ShaderExpr Number) where
  div e1 e2 = binary OpDiv e1 e2
  degree _ = 1
  mod _ _ = num 0.0 -- A proper lawful definition, but not a useful one for shader programming
