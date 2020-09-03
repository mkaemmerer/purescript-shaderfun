module Shader.Expr
  ( Expr(..)
  , Type(..)
  , UnaryExpr(..)
  , BinaryExpr(..)
  , CallExpr(..)
  , class TypedExpr
  , class VecExpr
  , class HasX
  , class HasY
  , class HasZ
  , abs
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
  , eq
  , floor
  , fract
  , fromUnit
  , fromBoolean
  , fromNumber
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
  , mod
  , neq
  , num
  , p
  , pow
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
  | TUnit
  | TTuple Type Type
  | TEither Type Type

data UnaryExpr t
  = UnNegate (Expr Number)
  | UnNot (Expr Boolean)
  | UnProjV2X (Expr Vec2)
  | UnProjV2Y (Expr Vec2)
  | UnProjV3X (Expr Vec3)
  | UnProjV3Y (Expr Vec3)
  | UnProjV3Z (Expr Vec3)
  | UnProjReal (Expr Complex)
  | UnProjImaginary (Expr Complex)
  | UnProjR (Expr Color)
  | UnProjG (Expr Color)
  | UnProjB (Expr Color)

data BinaryExpr t
  = BinEq (Expr Number) (Expr Number)
  | BinNeq (Expr Number) (Expr Number)
  | BinLt (Expr Number) (Expr Number)
  | BinLte (Expr Number) (Expr Number)
  | BinGt (Expr Number) (Expr Number)
  | BinGte (Expr Number) (Expr Number)
  | BinAnd (Expr Boolean) (Expr Boolean)
  | BinOr (Expr Boolean) (Expr Boolean)
  | BinPlus (Expr Number) (Expr Number)
  | BinMinus (Expr Number) (Expr Number)
  | BinTimes (Expr Number) (Expr Number)
  | BinDiv (Expr Number) (Expr Number)
  | BinPlusV2 (Expr Vec2) (Expr Vec2)
  | BinMinusV2 (Expr Vec2) (Expr Vec2)
  | BinScaleV2 (Expr Number) (Expr Vec2)
  | BinPlusV3 (Expr Vec3) (Expr Vec3)
  | BinMinusV3 (Expr Vec3) (Expr Vec3)
  | BinScaleV3 (Expr Number) (Expr Vec3)
  | BinPlusC (Expr Complex) (Expr Complex)
  | BinMinusC (Expr Complex) (Expr Complex)
  | BinTimesC (Expr Complex) (Expr Complex)
  | BinDivC (Expr Complex) (Expr Complex)
  | BinScaleC (Expr Number) (Expr Complex)
  | BinPlusCol (Expr Color) (Expr Color)
  | BinMinusCol (Expr Color) (Expr Color)
  | BinTimesCol (Expr Color) (Expr Color)
  | BinScaleCol (Expr Number) (Expr Color)

data CallExpr t
  = FnAbs (Expr Number)
  | FnCos (Expr Number)
  | FnFloor (Expr Number)
  | FnFract (Expr Number)
  | FnLog (Expr Number)
  | FnLog2 (Expr Number)
  | FnSaturate (Expr Number)
  | FnSin (Expr Number)
  | FnSqrt (Expr Number)
  | FnAtan (Expr Number) (Expr Number)
  | FnMax (Expr Number) (Expr Number)
  | FnMin (Expr Number) (Expr Number)
  | FnMod (Expr Number) (Expr Number)
  | FnPow (Expr Number) (Expr Number)
  | FnSmoothstep (Expr Number) (Expr Number) (Expr Number)
  | FnLengthV2 (Expr Vec2)
  | FnLengthV3 (Expr Vec3)
  | FnDotV2 (Expr Vec2) (Expr Vec2)
  | FnDotV3 (Expr Vec3) (Expr Vec3)
  | FnDotC (Expr Complex) (Expr Complex)
  | FnReflectV2 (Expr Vec2) (Expr Vec2)
  | FnReflectV3 (Expr Vec3) (Expr Vec3)

data Expr t
  = EVar String
  | EBool Boolean
  | ENum Number
  | EVec2 (Expr Number) (Expr Number)
  | EVec3 (Expr Number) (Expr Number) (Expr Number)
  | EComplex (Expr Number) (Expr Number)
  | EColor (Expr Number) (Expr Number) (Expr Number)
  | EUnary (UnaryExpr t)
  | EBinary (BinaryExpr t)
  | EUnit
  | ETuple (forall a. Expr a) (forall a. Expr a)
  | EFst (Expr (forall a. Tuple t a))
  | ESnd (Expr (forall a. Tuple a t))
  | EMatch (Expr (forall a b. Either a b)) String (Expr t) String (Expr t)
  | EInl (Expr (forall a. Either t a))
  | EInr (Expr (forall a. Either a t))
  | ECall (CallExpr t)
  | EIf (Expr Boolean) (Expr t) (Expr t)
  | EBind String Type (forall a. Expr a) (Expr t)

derive instance eqType :: Eq Type
derive instance eqExpr :: Eq (Expr a)
derive instance eqUnaryExpr :: Eq (UnaryExpr a)
derive instance eqBinaryExpr :: Eq (BinaryExpr a)
derive instance eqCallExpr :: Eq (CallExpr a)


type Tag = Expr Boolean

tagLeft :: Tag
tagLeft = tt

tagRight :: Tag
tagRight = ff


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

instance typedExprUnit :: TypedExpr Unit where
  typeof e = TUnit

instance typedExprExprTuple :: (TypedExpr a, TypedExpr b) => TypedExpr (Tuple a b) where
  typeof e = TTuple (typeof $ fst e) (typeof $ snd e)

instance typedExprExprEither :: (TypedExpr a, TypedExpr b) => TypedExpr (Either a b) where
  typeof e = TEither (typeof $ asLeft e) (typeof $ asRight e)
    where
      asLeft :: Expr (Either a b) -> (Expr a)
      asLeft ex = unsafeCoerce ex
      asRight :: Expr (Either a b) -> (Expr b)
      asRight ex = unsafeCoerce ex

-- Unsafe, private constructors
eraseType :: forall a b. Expr a -> Expr b
eraseType = unsafeCoerce

unary :: forall a b. (Expr a -> UnaryExpr b) -> Expr a -> Expr b
unary mkUnary e = EUnary (mkUnary e)

binary :: forall a b c. (Expr a -> Expr b -> BinaryExpr c) -> Expr a -> Expr b -> Expr c
binary mkBinary e1 e2 = EBinary (mkBinary e1 e2)

call :: forall a b. (Expr a -> CallExpr b) -> Expr a -> Expr b
call mkCall a = ECall (mkCall a)

call2 :: forall a b c. (Expr a -> Expr b -> CallExpr c) -> Expr a -> Expr b -> Expr c
call2 mkCall a b = ECall (mkCall a b)

call3 :: forall a b c d. (Expr a -> Expr b -> Expr c -> CallExpr d) -> Expr a -> Expr b -> Expr c -> Expr d
call3 mkCall a b c = ECall (mkCall a b c)

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

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

inl :: forall a b. Expr a -> Expr (Either a b)
inl a = EInl (eraseType a)

inr :: forall a b. Expr b -> Expr (Either a b)
inr b = EInr (eraseType b)

ifE :: forall a. Expr Boolean -> Expr a -> Expr a -> Expr a
ifE i t e = EIf i t e

bindE :: forall s t. (TypedExpr s) => String -> Expr s -> Expr t -> Expr t
bindE name e1 e2 = EBind name (typeof e1) (eraseType e1) e2

matchE :: forall a b c. Expr (Either a b) -> String -> (Expr a -> Expr c) -> (Expr b -> Expr c) -> Expr c
matchE e name lBranch rBranch = EMatch (eraseType e) name_l expr_l name_r expr_r
  where
    name_l = name <> "_l"
    name_r = name <> "_r"
    expr_l = lBranch (EVar name_l)
    expr_r = rBranch (EVar name_r)

fromUnit :: Unit -> Expr Unit
fromUnit _ = EUnit

fromBoolean :: Boolean -> Expr Boolean
fromBoolean = bool

fromNumber :: Number -> Expr Number
fromNumber = num

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
  ) <= VecExpr v where
  length :: Expr v -> Expr Number
  reflect :: Expr v -> Expr v -> Expr v

class (VecExpr t) <= HasX t where
  projX :: Expr t -> Expr Number

class (VecExpr t) <= HasY t where
  projY :: Expr t -> Expr Number

class (VecExpr t) <= HasZ t where
  projZ :: Expr t -> Expr Number

instance vecExprVec2 :: VecExpr Vec2 where
  length = lengthV2
  reflect = reflectV2

instance vecExprVec3 :: VecExpr Vec3 where
  length = lengthV3
  reflect = reflectV3

instance hasXVec2 :: HasX Vec2 where
  projX = unary UnProjV2X

instance hasYVec2 :: HasY Vec2 where
  projY = unary UnProjV2Y

instance hasXVec3 :: HasX Vec3 where
  projX = unary UnProjV3X
  
instance hasYVec3 :: HasY Vec3 where
  projY = unary UnProjV3Y
  
instance hasZVec3 :: HasZ Vec3 where
  projZ = unary UnProjV3Z
  

projReal :: Expr Complex -> Expr Number
projReal = unary UnProjReal

projImaginary :: Expr Complex -> Expr Number
projImaginary = unary UnProjImaginary

projR :: Expr Color -> Expr Number
projR = unary UnProjR

projG :: Expr Color -> Expr Number
projG = unary UnProjG

projB :: Expr Color -> Expr Number
projB = unary UnProjB


-- Built-in functions
-- Boolean functions
lt :: Expr Number -> Expr Number -> Expr Boolean
lt = binary BinLt

lte :: Expr Number -> Expr Number -> Expr Boolean
lte = binary BinLte

gt :: Expr Number -> Expr Number -> Expr Boolean
gt = binary BinGt

gte :: Expr Number -> Expr Number -> Expr Boolean
gte = binary BinGte

eq :: Expr Number -> Expr Number -> Expr Boolean
eq = binary BinEq

neq :: Expr Number -> Expr Number -> Expr Boolean
neq = binary BinNeq

-- Scalar functions
abs :: Expr Number -> Expr Number
abs = call FnAbs

cos :: Expr Number -> Expr Number
cos = call FnCos

floor :: Expr Number -> Expr Number
floor = call FnFloor

fract :: Expr Number -> Expr Number
fract = call FnFract

log :: Expr Number -> Expr Number
log = call FnLog

log2 :: Expr Number -> Expr Number
log2 = call FnLog2

saturate :: Expr Number -> Expr Number
saturate = call FnSaturate

sin :: Expr Number -> Expr Number
sin = call FnSin

sqrt :: Expr Number -> Expr Number
sqrt = call FnSqrt

atan :: Expr Number -> Expr Number -> Expr Number
atan = call2 FnAtan

max :: Expr Number -> Expr Number -> Expr Number
max = call2 FnMax

min :: Expr Number -> Expr Number -> Expr Number
min = call2 FnMin

mod :: Expr Number -> Expr Number -> Expr Number
mod = call2 FnMod

pow :: Expr Number -> Expr Number -> Expr Number
pow = call2 FnPow

smoothstep :: Expr Number -> Expr Number -> Expr Number -> Expr Number
smoothstep = call3 FnSmoothstep

-- Vector functions
lengthV2 :: Expr Vec2 -> Expr Number
lengthV2 = call FnLengthV2

lengthV3 :: Expr Vec3 -> Expr Number
lengthV3 = call FnLengthV3

reflectV2 :: Expr Vec2 -> Expr Vec2 -> Expr Vec2
reflectV2 = call2 FnReflectV2

reflectV3 :: Expr Vec3 -> Expr Vec3 -> Expr Vec3
reflectV3 = call2 FnReflectV3

dotV2 :: Expr Vec2 -> Expr Vec2 -> Expr Number
dotV2 = call2 FnDotV2

dotV3 :: Expr Vec3 -> Expr Vec3 -> Expr Number
dotV3  = call2 FnDotV3

dotC :: Expr Complex -> Expr Complex -> Expr Number
dotC  = call2 FnDotC


-- Useful Instances
-- Bool
instance heytingAlgebraExprBool :: HeytingAlgebra (Expr Boolean) where
  ff = bool false
  tt = bool true
  disj = binary BinOr
  conj = binary BinAnd
  not = unary UnNot
  implies a b = b || (not a)

instance booleanAlgebraExprBool :: BooleanAlgebra (Expr Boolean)


-- Number
instance semiringExprNumber :: Semiring (Expr Number) where
  zero = num zero
  one = num one
  add = binary BinPlus
  mul = binary BinTimes

instance ringExprNumber :: Ring (Expr Number) where
  sub = binary BinMinus

instance commutativeRingExprNumber :: CommutativeRing (Expr Number)

instance euclideanRingExprNumber :: EuclideanRing (Expr Number) where
  div = binary BinDiv
  degree _ = 1
  mod _ _ = num zero -- A proper lawful definition, but not a useful one for shader programming

instance divisionRingExprNumber :: DivisionRing (Expr Number) where
  recip e = one / e

instance additevGroupExprNumber :: AdditiveGroup (Expr Number) where
  zeroV = zero
  addV = add
  subV = sub
  negateV = unary UnNegate

instance vectorSpaceExprNumber :: VectorSpace (Expr Number) (Expr Number) where
  scale = mul

instance innerSpaceExprNumber :: InnerSpace (Expr Number) (Expr Number) where
  dot = mul


-- Complex
instance semiringExprComplex :: Semiring (Expr Complex) where
  zero = fromComplex zero
  one = fromComplex one
  add = binary BinPlusC
  mul = binary BinTimesC

instance ringExprComplex :: Ring (Expr Complex) where
  sub = binary BinMinusC

instance commutativeRingExprComplex :: CommutativeRing (Expr Complex)

instance euclideanRingExprComplex :: EuclideanRing (Expr Complex) where
  degree _ = 1
  mod _ _ = fromComplex zero -- A proper lawful definition, but not a useful one for shader programming
  div = binary BinDivC

instance divisionRingExprComplex :: DivisionRing (Expr Complex) where
  recip e = one / e

instance additiveGroupExprComplex :: AdditiveGroup (Expr Complex) where
  zeroV = zero
  addV = add
  subV = sub
  negateV = binary BinScaleC (num (-1.0))

instance vectorSpaceExprComplex :: VectorSpace (Expr Number) (Expr Complex) where
  scale = binary BinScaleC

instance innerSpaceExprComplex :: InnerSpace (Expr Number) (Expr Complex) where
  dot = dotC


-- Vec2
instance additiveGroupExprVec2 :: AdditiveGroup (Expr Vec2) where
  zeroV = fromVec2 zeroV
  addV = binary BinPlusV2
  subV = binary BinMinusV2
  negateV = binary BinScaleV2 (num (-1.0))

instance vectorSpaceExprVec2 :: VectorSpace (Expr Number) (Expr Vec2) where
  scale = binary BinScaleV2

instance innerSpaceExprVec2 :: InnerSpace (Expr Number) (Expr Vec2) where
  dot = dotV2


-- Vec3
instance additiveGroupExprVec3 :: AdditiveGroup (Expr Vec3) where
  zeroV = fromVec3 zeroV
  addV = binary BinPlusV3
  subV = binary BinMinusV3
  negateV = binary BinScaleV3 (num (-1.0))

instance vectorSpaceExprVec3 :: VectorSpace (Expr Number) (Expr Vec3) where
  scale = binary BinScaleV3

instance innerSpaceExprVec3 :: InnerSpace (Expr Number) (Expr Vec3) where
  dot = dotV3


-- Color
instance semiringExprColor :: Semiring (Expr Color) where
  zero = fromColor zero
  one = fromColor one
  add = binary BinPlusCol
  mul = binary BinTimesCol

instance ringExprColor :: Ring (Expr Color) where
  sub = binary BinMinusCol

instance additiveGroupExprColor :: AdditiveGroup (Expr Color) where
  zeroV = fromColor zeroV
  addV = binary BinPlusCol
  subV = binary BinMinusCol
  negateV = binary BinScaleCol (num (-1.0))

instance vectorSpaceExprColor :: VectorSpace (Expr Number) (Expr Color) where
  scale = binary BinScaleCol

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
