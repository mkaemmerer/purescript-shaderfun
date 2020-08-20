module Shader.Expr
  ( Expr(..)
  , Type(..)
  , UnaryOp(..)
  , BinaryOp(..)
  , class TypedExpr
  , typeof
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
  , eq
  , neq
  , lt
  , lte
  , gt
  , gte
  , ifE
  , bindE
  , fromVec2
  , fromColor
  ) where

import Prelude

import Data.Color (class ColorSpace, Color(..))
import Data.Vec2 (Vec2(..))
import Data.VectorSpace (class AdditiveGroup, class VectorSpace, class InnerSpace, zeroV)
import Unsafe.Coerce (unsafeCoerce)

data Type
  = TBoolean
  | TScalar
  | TVec2
  | TColor

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

data Expr t
  = EVar String
  | ENum Number
  | EBool Boolean
  | EVec2 (Expr Number) (Expr Number)
  | EColor (Expr Number) (Expr Number) (Expr Number)
  | EUnary UnaryOp (forall a. Expr a)
  | EBinary BinaryOp (forall a. Expr a) (forall a. Expr a)
  | EParen (Expr t)
  | ECall String (forall a. Array (Expr a))
  | EIf (Expr Boolean) (Expr t) (Expr t)
  | EBind String Type (forall a. Expr a) (Expr t)


class TypedExpr a where
  typeof :: a -> Type

instance typedExprExprBoolean :: TypedExpr (Expr Boolean) where
  typeof e = TBoolean

instance typedExprExprNumber :: TypedExpr (Expr Number) where
  typeof e = TScalar

instance typedExprExprVec2 :: TypedExpr (Expr Vec2) where
  typeof e = TVec2

instance typedExprExprColor :: TypedExpr (Expr Color) where
  typeof e = TColor

-- Unsafe, private constructors
eraseType :: forall a b. Expr a -> Expr b
eraseType = unsafeCoerce

unary :: forall a b. UnaryOp -> Expr a -> Expr b
unary op e = EUnary op (eraseType e)

binary :: forall a b c. BinaryOp -> Expr a -> Expr b -> Expr c
binary op l r = EBinary op (eraseType l) (eraseType r)

call :: forall t. String -> (forall a. Array (Expr a)) -> Expr t
call fn args = ECall fn args

-- Smart constructors
num :: Number -> Expr Number
num n = ENum n

bool :: Boolean -> Expr Boolean
bool b = EBool b

vec2 :: Expr Number -> Expr Number -> Expr Vec2
vec2 x y = EVec2 x y

color :: Expr Number -> Expr Number -> Expr Number -> Expr Color
color r g b = EColor r g b

paren :: forall a. Expr a -> Expr a
paren e = EParen e

ifE :: forall a. Expr Boolean -> Expr a -> Expr a -> Expr a
ifE i t e = EIf i t e

bindE :: forall s t. (TypedExpr (Expr s)) => String -> Expr s -> Expr t -> Expr t
bindE name e1 e2 = EBind name (typeof e1) (eraseType e1) e2

fromVec2 :: Vec2 -> Expr Vec2
fromVec2 (Vec2 x y) = vec2 (num x) (num y)

fromColor :: Color -> Expr Color
fromColor (Color r g b) = color (num r) (num g) (num b)

-- The input point. A variable that we have "blessed" as a vector
p :: Expr Vec2
p = EVar "p"

projX :: Expr Vec2 -> Expr Number
projX v = unary OpProjX v

projY :: Expr Vec2 -> Expr Number
projY v = unary OpProjY v

projR :: Expr Color -> Expr Number
projR c = unary OpProjR c

projG :: Expr Color -> Expr Number
projG c = unary OpProjG c

projB :: Expr Color -> Expr Number
projB c = unary OpProjB c


-- Built-in functions
-- Boolean functions
lt :: Expr Number -> Expr Number -> Expr Boolean
lt = binary OpLt

lte :: Expr Number -> Expr Number -> Expr Boolean
lte = binary OpLte

gt :: Expr Number -> Expr Number -> Expr Boolean
gt = binary OpGt

gte :: Expr Number -> Expr Number -> Expr Boolean
gte = binary OpGte

eq :: Expr Number -> Expr Number -> Expr Boolean
eq = binary OpEq

neq :: Expr Number -> Expr Number -> Expr Boolean
neq = binary OpNeq

-- Scalar functions
abs :: Expr Number -> Expr Number
abs n = call "abs" [ eraseType n ]

cos :: Expr Number -> Expr Number
cos n = call "sin" [ eraseType n ]

floor :: Expr Number -> Expr Number
floor n = call "floor" [ eraseType n ]

fract :: Expr Number -> Expr Number
fract n = call "fract" [ eraseType n ]

log :: Expr Number -> Expr Number
log n = call "log" [ eraseType n ]

log2 :: Expr Number -> Expr Number
log2 n = call "log2" [ eraseType n ]

saturate :: Expr Number -> Expr Number
saturate n = call "saturate" [ eraseType n ]

sin :: Expr Number -> Expr Number
sin n = call "sin" [ eraseType n ]

sqrt :: Expr Number -> Expr Number
sqrt n = call "sqrt" [ eraseType n ]

atan :: Expr Number -> Expr Number -> Expr Number
atan y x = call "atan2" [ eraseType y, eraseType x ]

max :: Expr Number -> Expr Number -> Expr Number
max x y = call "max" [ eraseType x, eraseType y ]

min :: Expr Number -> Expr Number -> Expr Number
min x y = call "min" [ eraseType x, eraseType y ]

mod :: Expr Number -> Expr Number -> Expr Number
mod x y = call "mod" [ eraseType x, eraseType y ]

smoothstep :: Expr Number -> Expr Number -> Expr Number -> Expr Number
smoothstep lo hi x = call "smoothstep" [ eraseType lo, eraseType hi, eraseType x ]

-- Vector functions
absV :: Expr Vec2 -> Expr Vec2
absV v = call "abs" [ eraseType v ]

length :: Expr Vec2 -> Expr Number
length v = call "length" [ eraseType v ]

dot :: Expr Vec2 -> Expr Vec2 -> Expr Number
dot u v = call "dot" [ eraseType u, eraseType v ]

-- Color functions
mix :: Expr Color -> Expr Color -> Expr Number -> Expr Color
mix c1 c2 fac = call "mix" [ eraseType c1, eraseType c2, eraseType fac ]


-- Useful Instances
-- Bool
instance heytingAlgebraExprBool :: HeytingAlgebra (Expr Boolean) where
  ff = bool false
  tt = bool true
  disj = binary OpOr
  conj = binary OpAnd
  not = unary OpNot
  implies a b = b || (not a)

instance booleanAlgebraExprBool :: BooleanAlgebra (Expr Boolean)

-- Number
instance semiringExprNumber :: Semiring (Expr Number) where
  zero = num 0.0
  one = num 1.0
  add = binary OpPlus
  mul = binary OpTimes

instance ringExprNumber :: Ring (Expr Number) where
  sub = binary OpMinus

instance divisionRingExprNumber :: DivisionRing (Expr Number) where
  recip e = binary OpDiv (num 1.0) e

instance commutativeRingExprNumber :: CommutativeRing (Expr Number)

instance euclideanRingExprNumber :: EuclideanRing (Expr Number) where
  div = binary OpDiv
  degree _ = 1
  mod _ _ = num 0.0 -- A proper lawful definition, but not a useful one for shader programming

-- Vec2
instance additiveGroupExprVec2 :: AdditiveGroup (Expr Vec2) where
  zeroV = fromVec2 zeroV
  addV = binary OpPlusV
  subV = binary OpMinusV
  negateV = binary OpScaleV (num (-1.0))

instance vectorSpaceExprVec2 :: VectorSpace (Expr Number) (Expr Vec2) where
  scale = binary OpScaleV

instance innerSpaceExprVec :: InnerSpace (Expr Number) (Expr Vec2) where
  dot u v = call "dot" [ unsafeCoerce u, unsafeCoerce v ]

-- Color
instance additiveGroupExprColor :: AdditiveGroup (Expr Color) where
  zeroV = fromColor zeroV
  addV = binary OpPlusC
  subV = binary OpMinusC
  negateV = binary OpScaleC (num (-1.0))

instance vectorSpaceExprColor :: VectorSpace (Expr Number) (Expr Color) where
  scale = binary OpScaleC

instance colorSpaceExprColor :: ColorSpace (Expr Number) (Expr Color) where
  timesC = binary OpTimesC
