module Shader.Expr
  ( ShaderExpr(..)
  , UnaryOp(..)
  , BinaryOp(..)
  , p
  , num
  , bool
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
  , fromVec2
  , fromColor
  ) where

import Prelude

import Data.Color (class ColorSpace, Color(..))
import Data.Leibniz (type (~))
import Data.Vec2 (Vec2(..))
import Data.VectorSpace (class AdditiveGroup, class VectorSpace, class InnerSpace, zeroV)
import Unsafe.Coerce (unsafeCoerce)


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
  | OpPlusV
  | OpMinusV
  | OpScaleV
  | OpPlusC
  | OpMinusC
  | OpTimesC
  | OpScaleC

data ShaderExpr t
  = EVar String
  | ENum Number (t ~ Number)
  | EBool Boolean (t ~ Boolean)
  | EVec2 (ShaderExpr Number) (ShaderExpr Number) (t ~ Vec2)
  | EColor (ShaderExpr Number) (ShaderExpr Number) (ShaderExpr Number) (t ~ Color)
  | EUnary UnaryOp (forall a. ShaderExpr a)
  | EBinary BinaryOp (forall a. ShaderExpr a) (forall a. ShaderExpr a)
  | EParen (ShaderExpr t)
  | ECall String (forall a. Array (ShaderExpr a))
-- | EIf (ShaderExpr Boolean) (ShaderExpr t) (ShaderExpr t)
-- | EBind

-- Unsafe, private constructors
unary :: forall a b. UnaryOp -> ShaderExpr a -> ShaderExpr b
unary op e = EUnary op (unsafeCoerce e)

binary :: forall a b c. BinaryOp -> ShaderExpr a -> ShaderExpr b -> ShaderExpr c
binary op l r = EBinary op (unsafeCoerce l) (unsafeCoerce r)

call :: forall t. String -> (forall a. Array (ShaderExpr a)) -> ShaderExpr t
call fn args = ECall fn args


-- Smart constructors
num :: Number -> ShaderExpr Number
num n = ENum n identity

bool :: Boolean -> ShaderExpr Boolean
bool b = EBool b identity

vec2 :: ShaderExpr Number -> ShaderExpr Number -> ShaderExpr Vec2
vec2 x y = EVec2 x y identity

color :: ShaderExpr Number -> ShaderExpr Number -> ShaderExpr Number -> ShaderExpr Color
color r g b = EColor r g b identity

paren :: forall a. ShaderExpr a -> ShaderExpr a
paren e = EParen e

fromVec2 :: Vec2 -> ShaderExpr Vec2
fromVec2 (Vec2 x y) = vec2 (num x) (num y)

fromColor :: Color -> ShaderExpr Color
fromColor (Color r g b) = color (num r) (num g) (num b)

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
-- Bool
instance heytingAlgebraShaderExprBool :: HeytingAlgebra (ShaderExpr Boolean) where
  ff = bool false
  tt = bool true
  disj = binary OpOr
  conj = binary OpAnd
  not = unary OpNot
  implies a b = b || (not a)

instance booleanAlgebraShaderExprBool :: BooleanAlgebra (ShaderExpr Boolean)

-- Number
instance semiringShaderExprNumber :: Semiring (ShaderExpr Number) where
  zero = num 0.0
  one = num 1.0
  add = binary OpPlus
  mul = binary OpTimes

instance ringShaderExprNumber :: Ring (ShaderExpr Number) where
  sub = binary OpMinus

instance divisionRingShaderExprNumber :: DivisionRing (ShaderExpr Number) where
  recip e = binary OpDiv (num 1.0) e

instance commutativeRingShaderExprNumber :: CommutativeRing (ShaderExpr Number)

instance euclideanRingShaderExprNumber :: EuclideanRing (ShaderExpr Number) where
  div = binary OpDiv
  degree _ = 1
  mod _ _ = num 0.0 -- A proper lawful definition, but not a useful one for shader programming

-- Vec2
instance additiveGroupShaderExprVec2 :: AdditiveGroup (ShaderExpr Vec2) where
  zeroV = fromVec2 zeroV
  addV = binary OpPlusV
  subV = binary OpMinusV
  negateV = binary OpScaleV (num (-1.0))

instance vectorSpaceShaderExprVec2 :: VectorSpace (ShaderExpr Number) (ShaderExpr Vec2) where
  scale = binary OpScaleV

instance innerSpaceShaderExprVec :: InnerSpace (ShaderExpr Number) (ShaderExpr Vec2) where
  dot u v = call "dot" [ unsafeCoerce u, unsafeCoerce v ]

-- Color
instance additiveGroupShaderExprColor :: AdditiveGroup (ShaderExpr Color) where
  zeroV = fromColor zeroV
  addV = binary OpPlusC
  subV = binary OpMinusC
  negateV = binary OpScaleC (num (-1.0))

instance vectorSpaceShaderExprColor :: VectorSpace (ShaderExpr Number) (ShaderExpr Color) where
  scale = binary OpScaleC

instance colorSpaceShaderExprColor :: ColorSpace (ShaderExpr Number) (ShaderExpr Color) where
  timesC = binary OpTimesC
