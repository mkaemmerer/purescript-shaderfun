module Shader.Expr
  ( Expr(..)
  , Type(..)
  , UnaryOp(..)
  , BinaryOp(..)
  , class TypedExpr
  , class VecExpr
  , class HasX
  , class HasY
  , class HasZ
  , abs
  , absV
  , atan
  , bindE
  , bool
  , color
  , complex
  , unit
  , tuple
  , fst
  , snd
  , inl
  , inr
  , matchE
  , cos
  , dotC
  , dotV
  , eq
  , floor
  , fract
  , fromColor
  , fromComplex
  , fromVec2
  , fromVec3
  , complexToVec2
  , vec2ToComplex
  , gt
  , gte
  , ifE
  , length
  , log
  , log2
  , lt
  , lte
  , max
  , min
  , mix
  , mod
  , neq
  , num
  , p
  , projX
  , projY
  , projZ
  , projReal
  , projImaginary
  , projR
  , projG
  , projB
  , reflect
  , saturate
  , sin
  , smoothstep
  , sqrt
  , typeof
  , vec2
  , vec3
  ) where

import Prelude

import Data.Color (Color(..))
import Data.Complex (Complex(..))
import Data.Either (Either)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Tuple (Tuple)
import Data.Vec2 (Vec2(..))
import Data.Vec3 (Vec3(..))
import Data.VectorSpace (class AdditiveGroup, class InnerSpace, class VectorSpace, negateV, zeroV, (*^), (^+^), (^-^), (<.>))
import Unsafe.Coerce (unsafeCoerce)

data Type
  = TBoolean
  | TScalar
  | TVec2
  | TVec3
  | TComplex
  | TColor

type Tag = Expr Boolean

tagLeft :: Tag
tagLeft = tt

tagRight :: Tag
tagRight = ff

data UnaryOp
  = OpNegate
  | OpNot
  | OpProjX
  | OpProjY
  | OpProjZ
  | OpProjReal
  | OpProjImaginary
  | OpProjR
  | OpProjG
  | OpProjB

data BinaryOp
  = OpEq
  | OpNeq
  | OpAnd
  | OpOr
  | OpLt
  | OpLte
  | OpGt
  | OpGte
  | OpPlus
  | OpMinus
  | OpTimes
  | OpDiv
  | OpPlusV
  | OpMinusV
  | OpScaleV
  | OpPlusC
  | OpMinusC
  | OpTimesC
  | OpDivC
  | OpScaleC
  | OpPlusCol
  | OpMinusCol
  | OpTimesCol
  | OpScaleCol

data Expr t
  = EVar String
  | EBool Boolean
  | ENum Number
  | EVec2 (Expr Number) (Expr Number)
  | EVec3 (Expr Number) (Expr Number) (Expr Number)
  | EComplex (Expr Number) (Expr Number)
  | EColor (Expr Number) (Expr Number) (Expr Number)
  | EUnary UnaryOp (forall a. Expr a)
  | EBinary BinaryOp (forall a. Expr a) (forall a. Expr a)
  | EUnit
  | ETuple (forall a. Expr a) (forall a. Expr a)
  | EFst (Expr (forall a. Tuple t a))
  | ESnd (Expr (forall a. Tuple a t))
  | EParen (Expr t)
  | ECall String (forall a. Array (Expr a))
  | EIf (Expr Boolean) (Expr t) (Expr t)
  | EBind String Type (forall a. Expr a) (Expr t)


class TypedExpr a where
  typeof :: (Expr a) -> Type

instance typedExprExprBoolean :: TypedExpr Boolean where
  typeof e = TBoolean

instance typedExprExprNumber :: TypedExpr Number where
  typeof e = TScalar

instance typedExprExprVec2 :: TypedExpr Vec2 where
  typeof e = TVec2

instance typedExprExprVec3 :: TypedExpr Vec3 where
  typeof e = TVec3

instance typedExprExprComplex :: TypedExpr Complex where
  typeof e = TComplex

instance typedExprExprColor :: TypedExpr Color where
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
bool :: Boolean -> Expr Boolean
bool b = EBool b

num :: Number -> Expr Number
num n = ENum n

vec2 :: Expr Number -> Expr Number -> Expr Vec2
vec2 x y = EVec2 x y

vec3 :: Expr Number -> Expr Number  -> Expr Number -> Expr Vec3
vec3 x y z = EVec3 x y z

complex :: Expr Number -> Expr Number -> Expr Complex
complex r i = EComplex r i

color :: Expr Number -> Expr Number -> Expr Number -> Expr Color
color r g b = EColor r g b

-- Product types
unit :: Expr Unit
unit = EUnit

tuple :: forall a b. Expr a -> Expr b -> Expr (Tuple a b)
tuple a b = ETuple (eraseType a) (eraseType b)

fst :: forall a b. Expr (Tuple a b) -> Expr a
fst t = EFst (eraseType t)

snd :: forall a b. Expr (Tuple a b) -> Expr b
snd t = ESnd (eraseType t)

-- Encode sum types using product types
-- TODO: can this be generalized with Generics/Type Reps?
inl :: forall a b. Expr a -> Expr (Either a b)
inl a = ETuple (eraseType tagLeft) (eraseType a)

inr :: forall a b. Expr b -> Expr (Either a b)
inr b = ETuple (eraseType tagRight) (eraseType b)

matchE :: forall a b c. Expr (Either a b) -> (Expr a -> Expr c) -> (Expr b -> Expr c) -> Expr c
matchE e lBranch rBranch = ifE tag lVal rVal
  where
    tag = EFst (eraseType e)
    lVal = lBranch $ ESnd (eraseType e)
    rVal = rBranch $ ESnd (eraseType e)

paren :: forall a. Expr a -> Expr a
paren e = EParen e

ifE :: forall a. Expr Boolean -> Expr a -> Expr a -> Expr a
ifE i t e = EIf i t e

bindE :: forall s t. (TypedExpr s) => String -> Expr s -> Expr t -> Expr t
bindE name e1 e2 = EBind name (typeof e1) (eraseType e1) e2

fromVec2 :: Vec2 -> Expr Vec2
fromVec2 (Vec2 v) = vec2 (num v.x) (num v.y)

fromVec3 :: Vec3 -> Expr Vec3
fromVec3 (Vec3 v) = vec3 (num v.x) (num v.y) (num v.z)

fromComplex :: Complex -> Expr Complex
fromComplex (Complex r i) = complex (num r) (num i)

fromColor :: Color -> Expr Color
fromColor (Color r g b) = color (num r) (num g) (num b)

complexToVec2 :: Expr Complex -> Expr Vec2
complexToVec2 = unsafeCoerce

vec2ToComplex :: Expr Vec2 -> Expr Complex
vec2ToComplex = unsafeCoerce

-- The input point. A variable that we have "blessed" as a vector
p :: Expr Vec2
p = EVar "p"

class
  ( TypedExpr v
  , VectorSpace (Expr Number) (Expr v)
  , InnerSpace (Expr Number) (Expr v)
  ) <= VecExpr v
class (VecExpr t) <= HasX t
class (VecExpr t) <= HasY t
class (VecExpr t) <= HasZ t

instance vecExprVec2 :: VecExpr Vec2
instance vecExprVec3 :: VecExpr Vec3
instance hasXVec2 :: HasX Vec2
instance hasYVec2 :: HasY Vec2
instance hasXVec3 :: HasX Vec3
instance hasYVec3 :: HasY Vec3
instance hasZVec3 :: HasZ Vec3

projX :: forall vec. (HasX vec) => Expr vec -> Expr Number
projX v = unary OpProjX v

projY :: forall vec. (HasY vec) => Expr vec -> Expr Number
projY v = unary OpProjY v

projZ :: forall vec. (HasZ vec) => Expr vec -> Expr Number
projZ v = unary OpProjZ v

projReal :: Expr Complex -> Expr Number
projReal c = unary OpProjReal c

projImaginary :: Expr Complex -> Expr Number
projImaginary c = unary OpProjImaginary c

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
cos n = call "cos" [ eraseType n ]

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
atan y x = call "atan" [ eraseType y, eraseType x ]

max :: Expr Number -> Expr Number -> Expr Number
max x y = call "max" [ eraseType x, eraseType y ]

min :: Expr Number -> Expr Number -> Expr Number
min x y = call "min" [ eraseType x, eraseType y ]

mod :: Expr Number -> Expr Number -> Expr Number
mod x y = call "mod" [ eraseType x, eraseType y ]

smoothstep :: Expr Number -> Expr Number -> Expr Number -> Expr Number
smoothstep lo hi x = call "smoothstep" [ eraseType lo, eraseType hi, eraseType x ]

-- Vector functions
absV :: forall v. VecExpr v => Expr v -> Expr v
absV v = call "abs" [ eraseType v ]

length :: forall v. VecExpr v => Expr v -> Expr Number
length v = call "length" [ eraseType v ]

dotV :: forall v. VecExpr v => Expr v -> Expr v -> Expr Number
dotV u v = call "dot" [ eraseType u, eraseType v ]

dotC :: Expr Complex -> Expr Complex -> Expr Number
dotC u v = call "dot" [ eraseType u, eraseType v ]

reflect :: forall v. VecExpr v => Expr v -> Expr v -> Expr v
reflect u v = call "reflect" [ eraseType u, eraseType v ]

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
  zero = num zero
  one = num one
  add = binary OpPlus
  mul = binary OpTimes

instance ringExprNumber :: Ring (Expr Number) where
  sub = binary OpMinus

instance commutativeRingExprNumber :: CommutativeRing (Expr Number)

instance euclideanRingExprNumber :: EuclideanRing (Expr Number) where
  div = binary OpDiv
  degree _ = 1
  mod _ _ = num zero -- A proper lawful definition, but not a useful one for shader programming

instance divisionRingExprNumber :: DivisionRing (Expr Number) where
  recip e = one / e

instance additevGroupExprNumber :: AdditiveGroup (Expr Number) where
  zeroV = zero
  addV = add
  subV = sub
  negateV = unary OpNegate

instance vectorSpaceExprNumber :: VectorSpace (Expr Number) (Expr Number) where
  scale = mul

instance innerSpaceExprNumber :: InnerSpace (Expr Number) (Expr Number) where
  dot = mul


-- Complex
instance semiringExprComplex :: Semiring (Expr Complex) where
  zero = fromComplex zero
  one = fromComplex one
  add = binary OpPlusC
  mul = binary OpTimesC

instance ringExprComplex :: Ring (Expr Complex) where
  sub = binary OpMinusC

instance commutativeRingExprComplex :: CommutativeRing (Expr Complex)

instance euclideanRingExprComplex :: EuclideanRing (Expr Complex) where
  degree _ = 1
  mod _ _ = fromComplex zero -- A proper lawful definition, but not a useful one for shader programming
  div = binary OpDivC

instance divisionRingExprComplex :: DivisionRing (Expr Complex) where
  recip e = one / e

instance additiveGroupExprComplex :: AdditiveGroup (Expr Complex) where
  zeroV = zero
  addV = add
  subV = sub
  negateV = binary OpScaleC (num (-1.0))

instance vectorSpaceExprComplex :: VectorSpace (Expr Number) (Expr Complex) where
  scale = binary OpScaleC

instance innerSpaceExprComplex :: InnerSpace (Expr Number) (Expr Complex) where
  dot = dotC


-- Vec2
instance additiveGroupExprVec2 :: AdditiveGroup (Expr Vec2) where
  zeroV = fromVec2 zeroV
  addV = binary OpPlusV
  subV = binary OpMinusV
  negateV = binary OpScaleV (num (-1.0))

instance vectorSpaceExprVec2 :: VectorSpace (Expr Number) (Expr Vec2) where
  scale = binary OpScaleV

instance innerSpaceExprVec2 :: InnerSpace (Expr Number) (Expr Vec2) where
  dot = dotV


-- Vec3
instance additiveGroupExprVec3 :: AdditiveGroup (Expr Vec3) where
  zeroV = fromVec3 zeroV
  addV = binary OpPlusV
  subV = binary OpMinusV
  negateV = binary OpScaleV (num (-1.0))

instance vectorSpaceExprVec3 :: VectorSpace (Expr Number) (Expr Vec3) where
  scale = binary OpScaleV

instance innerSpaceExprVec3 :: InnerSpace (Expr Number) (Expr Vec3) where
  dot = dotV


-- Color
instance semiringExprColor :: Semiring (Expr Color) where
  zero = fromColor zero
  one = fromColor one
  add = binary OpPlusCol
  mul = binary OpTimesCol

instance ringExprColor :: Ring (Expr Color) where
  sub = binary OpMinusCol

instance additiveGroupExprColor :: AdditiveGroup (Expr Color) where
  zeroV = fromColor zeroV
  addV = binary OpPlusCol
  subV = binary OpMinusCol
  negateV = binary OpScaleCol (num (-1.0))

instance vectorSpaceExprColor :: VectorSpace (Expr Number) (Expr Color) where
  scale = binary OpScaleCol

-- Tuple
instance semiringExprTuple :: (Semiring (Expr a), Semiring (Expr b)) => Semiring (Expr (Tuple a b)) where
  zero = tuple zero zero
  one = tuple one one
  add e1 e2 = tuple (fst e1 + fst e2) (snd e1 + snd e2)
  mul e1 e2 = tuple (fst e1 * fst e2) (snd e1 * snd e2)

instance ringExprTuple :: (Ring (Expr a), Ring (Expr b)) => Ring (Expr (Tuple a b)) where
  sub e1 e2 = tuple (fst e1 - fst e2) (snd e1 - snd e2) 

instance commutativeRingExprTuple :: (CommutativeRing (Expr a), CommutativeRing (Expr b)) => CommutativeRing (Expr (Tuple a b))

instance additiveGroupExprTuple :: (AdditiveGroup (Expr a), AdditiveGroup (Expr b)) => AdditiveGroup (Expr (Tuple a b)) where
  zeroV = tuple zeroV zeroV
  addV e1 e2 = tuple (fst e1 ^+^ fst e2) (snd e1 ^+^ snd e2)
  subV e1 e2 = tuple (fst e1 ^-^ fst e2) (snd e1 ^-^ snd e2)
  negateV e = tuple (negateV $ fst e) (negateV $ snd e)

instance vectorSpaceExprTuple :: (VectorSpace s (Expr a), VectorSpace s (Expr b)) => VectorSpace s (Expr (Tuple a b)) where
  scale s e = tuple (s *^ fst e) (s *^ snd e)

instance innerSpaceExprTuple :: (InnerSpace s (Expr a), InnerSpace s (Expr b)) => InnerSpace s (Expr (Tuple a b)) where
  dot u v = (fst u <.> fst v) ^+^ (snd u <.> snd v)

instance heytingAlgebraExprTuple :: (HeytingAlgebra (Expr a), HeytingAlgebra (Expr b)) => HeytingAlgebra (Expr (Tuple a b)) where
  ff = tuple ff ff
  tt = tuple tt tt
  not e = tuple (not $ fst e) (not $ snd e)
  disj e1 e2 = tuple (fst e1 || fst e2) (snd e1 || snd e2)
  conj e1 e2 = tuple (fst e1 && fst e2) (snd e1 && snd e2)
  implies e1 e2 = tuple (fst e1 `implies` fst e2) (snd e1 `implies` snd e2)

instance booleanAlgebraExprTuple :: (BooleanAlgebra (Expr a), BooleanAlgebra (Expr b)) => BooleanAlgebra (Expr (Tuple a b))
