module Shader.Expr.Traversal
  ( on
  , onA
  , over
  , Traversal
  , fromGeneric
  , foldExpr
  , foldVars
  ) where


import Prelude

import Data.Color (Color)
import Data.Complex (Complex)
import Data.Vec2 (Vec2)
import Data.Vec3 (Vec3)
import Shader.Expr (BinaryExpr(..), CallExpr(..), Expr(..), UnaryExpr(..))
import Shader.Expr.Cast (asBoolean, asColor, asComplex, asNumber, asVec2, asVec3)
import Unsafe.Coerce (unsafeCoerce)

eraseType :: forall a b. Expr a -> Expr b
eraseType = unsafeCoerce

-- | Lift a function over one level of an expression tree
on :: forall a. (forall b. Expr b -> Expr b) -> Expr a -> Expr a
on f (EVar n)           = EVar n
on f (EBool b)          = EBool b
on f (ENum n)           = ENum n
on f (EVec2 x y)        = EVec2 (f x) (f y)
on f (EVec3 x y z)      = EVec3 (f x) (f y) (f z)
on f (EComplex r i)     = EComplex (f r) (f i)
on f (EColor r g b)     = EColor (f r) (f g) (f b)
on f (EUnary e)         = EUnary $ onUnary f e
on f (EBinary e)        = EBinary $ onBinary f e
on f (ECall e)          = ECall $ onCall f e
on f (EUnit)            = EUnit
on f (ETuple a b)       = ETuple (f a) (f b)
on f (EFst tup)         = EFst (f tup)
on f (ESnd tup)         = ESnd (f tup)
on f (EIf i thn els)    = EIf (f i) (f thn) (f els)
on f (EInl val)         = EInl (f val)
on f (EInr val)         = EInr (f val)
on f (EMatch e lname l rname r)  = EMatch (f e) lname (f l) rname (f r)
on f (ERec n name e1 e2)         = ERec n name (f e1) (f e2)
on f (EBind name e1 e2)          = EBind name (f e1) (f e2)

onUnary :: forall a. (forall b. Expr b -> Expr b) -> UnaryExpr a -> UnaryExpr a
onUnary f (UnNegate e)        = UnNegate        (f e)
onUnary f (UnNot e)           = UnNot           (f e)
onUnary f (UnProjV2X e)       = UnProjV2X       (f e)
onUnary f (UnProjV2Y e)       = UnProjV2Y       (f e)
onUnary f (UnProjV3X e)       = UnProjV3X       (f e)
onUnary f (UnProjV3Y e)       = UnProjV3Y       (f e)
onUnary f (UnProjV3Z e)       = UnProjV3Z       (f e)
onUnary f (UnProjReal e)      = UnProjReal      (f e)
onUnary f (UnProjImaginary e) = UnProjImaginary (f e)
onUnary f (UnProjR e)         = UnProjR         (f e)
onUnary f (UnProjG e)         = UnProjG         (f e)
onUnary f (UnProjB e)         = UnProjB         (f e)

onBinary :: forall a. (forall b. Expr b -> Expr b) -> BinaryExpr a -> BinaryExpr a
onBinary f (BinEq e1 e2)       = BinEq       (f e1) (f e2)
onBinary f (BinNeq e1 e2)      = BinNeq      (f e1) (f e2)
onBinary f (BinLt e1 e2)       = BinLt       (f e1) (f e2)
onBinary f (BinLte e1 e2)      = BinLte      (f e1) (f e2)
onBinary f (BinGt e1 e2)       = BinGt       (f e1) (f e2)
onBinary f (BinGte e1 e2)      = BinGte      (f e1) (f e2)
onBinary f (BinAnd e1 e2)      = BinAnd      (f e1) (f e2)
onBinary f (BinOr e1 e2)       = BinOr       (f e1) (f e2)
onBinary f (BinPlus e1 e2)     = BinPlus     (f e1) (f e2)
onBinary f (BinMinus e1 e2)    = BinMinus    (f e1) (f e2)
onBinary f (BinTimes e1 e2)    = BinTimes    (f e1) (f e2)
onBinary f (BinDiv e1 e2)      = BinDiv      (f e1) (f e2)
onBinary f (BinPlusV2 e1 e2)   = BinPlusV2   (f e1) (f e2)
onBinary f (BinMinusV2 e1 e2)  = BinMinusV2  (f e1) (f e2)
onBinary f (BinScaleV2 e1 e2)  = BinScaleV2  (f e1) (f e2)
onBinary f (BinPlusV3 e1 e2)   = BinPlusV3   (f e1) (f e2)
onBinary f (BinMinusV3 e1 e2)  = BinMinusV3  (f e1) (f e2)
onBinary f (BinScaleV3 e1 e2)  = BinScaleV3  (f e1) (f e2)
onBinary f (BinPlusC e1 e2)    = BinPlusC    (f e1) (f e2)
onBinary f (BinMinusC e1 e2)   = BinMinusC   (f e1) (f e2)
onBinary f (BinTimesC e1 e2)   = BinTimesC   (f e1) (f e2)
onBinary f (BinDivC e1 e2)     = BinDivC     (f e1) (f e2)
onBinary f (BinScaleC e1 e2)   = BinScaleC   (f e1) (f e2)
onBinary f (BinPlusCol e1 e2)  = BinPlusCol  (f e1) (f e2)
onBinary f (BinMinusCol e1 e2) = BinMinusCol (f e1) (f e2)
onBinary f (BinTimesCol e1 e2) = BinTimesCol (f e1) (f e2)
onBinary f (BinScaleCol e1 e2) = BinScaleCol (f e1) (f e2)

onCall :: forall a. (forall b. Expr b -> Expr b) -> CallExpr a -> CallExpr a
onCall f (FnAbs e)               = FnAbs         (f e)
onCall f (FnCos e)               = FnCos         (f e)
onCall f (FnFloor e)             = FnFloor       (f e)
onCall f (FnFract e)             = FnFract       (f e)
onCall f (FnLog e)               = FnLog         (f e)
onCall f (FnLog2 e)              = FnLog2        (f e)
onCall f (FnSaturate e)          = FnSaturate    (f e)
onCall f (FnSin e)               = FnSin         (f e)
onCall f (FnSqrt e)              = FnSqrt        (f e)
onCall f (FnAtan e1 e2)          = FnAtan        (f e1) (f e2)
onCall f (FnMax e1 e2)           = FnMax         (f e1) (f e2)
onCall f (FnMin e1 e2)           = FnMin         (f e1) (f e2)
onCall f (FnMod e1 e2)           = FnMod         (f e1) (f e2)
onCall f (FnPow e1 e2)           = FnPow         (f e1) (f e2)
onCall f (FnSmoothstep e1 e2 e3) = FnSmoothstep  (f e1) (f e2) (f e3)
onCall f (FnLengthV2 e)          = FnLengthV2    (f e)
onCall f (FnLengthV3 e)          = FnLengthV3    (f e)
onCall f (FnNormalizeV2 e)       = FnNormalizeV2 (f e)
onCall f (FnNormalizeV3 e)       = FnNormalizeV3 (f e)
onCall f (FnDotV2 e1 e2)         = FnDotV2       (f e1) (f e2)
onCall f (FnDotV3 e1 e2)         = FnDotV3       (f e1) (f e2)
onCall f (FnDotC e1 e2)          = FnDotC        (f e1) (f e2)
onCall f (FnReflectV2 e1 e2)     = FnReflectV2   (f e1) (f e2)
onCall f (FnReflectV3 e1 e2)     = FnReflectV3   (f e1) (f e2)

-- | Lift an applicative function over one level of an expression tree
onA :: forall a f. Applicative f => (forall b. Expr b -> f (Expr b)) -> Expr a -> f (Expr a)
onA f (EVar n)           = pure $ EVar n
onA f (EBool b)          = pure $ EBool b
onA f (ENum n)           = pure $ ENum n
onA f (EVec2 x y)        = EVec2    <$> f x <*> f y
onA f (EVec3 x y z)      = EVec3    <$> f x <*> f y <*> f z
onA f (EComplex r i)     = EComplex <$> f r <*> f i
onA f (EColor r g b)     = EColor   <$> f r <*> f g <*> f b
onA f (EUnary e)         = EUnary   <$> onUnaryA f e
onA f (EBinary e)        = EBinary  <$> onBinaryA f e
onA f (ECall e)          = ECall    <$> onCallA f e
onA f (EUnit)            = pure EUnit
onA f (ETuple a b)       = (unsafeCoerce ETuple) <$> f a <*> f b
onA f (EFst tup)         = EFst <$> f tup
onA f (ESnd tup)         = ESnd <$> f tup
onA f (EIf i thn els)    = EIf  <$> f i <*> f thn <*> f els
onA f (EInl val)         = EInl <$> f val
onA f (EInr val)         = EInr <$> f val
onA f (EMatch e lname l rname r)  = EMatch <$> f e <*> pure lname <*> f l <*> pure rname <*> f r
onA f (ERec n name e1 e2)         = ERec <$> pure n <*> pure name <*> f e1 <*> f e2
onA f (EBind name e1 e2)          = (unsafeCoerce EBind) <$> pure name <*> f e1 <*> f e2

onUnaryA :: forall a f. Applicative f => (forall b. Expr b -> f (Expr b)) -> UnaryExpr a -> f (UnaryExpr a)
onUnaryA f (UnNegate e)        = UnNegate        <$> f e
onUnaryA f (UnNot e)           = UnNot           <$> f e
onUnaryA f (UnProjV2X e)       = UnProjV2X       <$> f e
onUnaryA f (UnProjV2Y e)       = UnProjV2Y       <$> f e
onUnaryA f (UnProjV3X e)       = UnProjV3X       <$> f e
onUnaryA f (UnProjV3Y e)       = UnProjV3Y       <$> f e
onUnaryA f (UnProjV3Z e)       = UnProjV3Z       <$> f e
onUnaryA f (UnProjReal e)      = UnProjReal      <$> f e
onUnaryA f (UnProjImaginary e) = UnProjImaginary <$> f e
onUnaryA f (UnProjR e)         = UnProjR         <$> f e
onUnaryA f (UnProjG e)         = UnProjG         <$> f e
onUnaryA f (UnProjB e)         = UnProjB         <$> f e

onBinaryA :: forall a f. Applicative f => (forall b. Expr b -> f (Expr b)) -> BinaryExpr a -> f (BinaryExpr a)
onBinaryA f (BinEq e1 e2)       = BinEq       <$> f e1  <*> f e2
onBinaryA f (BinNeq e1 e2)      = BinNeq      <$> f e1  <*> f e2
onBinaryA f (BinLt e1 e2)       = BinLt       <$> f e1  <*> f e2
onBinaryA f (BinLte e1 e2)      = BinLte      <$> f e1  <*> f e2
onBinaryA f (BinGt e1 e2)       = BinGt       <$> f e1  <*> f e2
onBinaryA f (BinGte e1 e2)      = BinGte      <$> f e1  <*> f e2
onBinaryA f (BinAnd e1 e2)      = BinAnd      <$> f e1  <*> f e2
onBinaryA f (BinOr e1 e2)       = BinOr       <$> f e1  <*> f e2
onBinaryA f (BinPlus e1 e2)     = BinPlus     <$> f e1  <*> f e2
onBinaryA f (BinMinus e1 e2)    = BinMinus    <$> f e1  <*> f e2
onBinaryA f (BinTimes e1 e2)    = BinTimes    <$> f e1  <*> f e2
onBinaryA f (BinDiv e1 e2)      = BinDiv      <$> f e1  <*> f e2
onBinaryA f (BinPlusV2 e1 e2)   = BinPlusV2   <$> f e1  <*> f e2
onBinaryA f (BinMinusV2 e1 e2)  = BinMinusV2  <$> f e1  <*> f e2
onBinaryA f (BinScaleV2 e1 e2)  = BinScaleV2  <$> f e1  <*> f e2
onBinaryA f (BinPlusV3 e1 e2)   = BinPlusV3   <$> f e1  <*> f e2
onBinaryA f (BinMinusV3 e1 e2)  = BinMinusV3  <$> f e1  <*> f e2
onBinaryA f (BinScaleV3 e1 e2)  = BinScaleV3  <$> f e1  <*> f e2
onBinaryA f (BinPlusC e1 e2)    = BinPlusC    <$> f e1  <*> f e2
onBinaryA f (BinMinusC e1 e2)   = BinMinusC   <$> f e1  <*> f e2
onBinaryA f (BinTimesC e1 e2)   = BinTimesC   <$> f e1  <*> f e2
onBinaryA f (BinDivC e1 e2)     = BinDivC     <$> f e1  <*> f e2
onBinaryA f (BinScaleC e1 e2)   = BinScaleC   <$> f e1  <*> f e2
onBinaryA f (BinPlusCol e1 e2)  = BinPlusCol  <$> f e1  <*> f e2
onBinaryA f (BinMinusCol e1 e2) = BinMinusCol <$> f e1  <*> f e2
onBinaryA f (BinTimesCol e1 e2) = BinTimesCol <$> f e1  <*> f e2
onBinaryA f (BinScaleCol e1 e2) = BinScaleCol <$> f e1  <*> f e2

onCallA :: forall a f. Applicative f => (forall b. Expr b -> f (Expr b)) -> CallExpr a -> f (CallExpr a)
onCallA f (FnAbs e)               = FnAbs         <$> f e
onCallA f (FnCos e)               = FnCos         <$> f e
onCallA f (FnFloor e)             = FnFloor       <$> f e
onCallA f (FnFract e)             = FnFract       <$> f e
onCallA f (FnLog e)               = FnLog         <$> f e
onCallA f (FnLog2 e)              = FnLog2        <$> f e
onCallA f (FnSaturate e)          = FnSaturate    <$> f e
onCallA f (FnSin e)               = FnSin         <$> f e
onCallA f (FnSqrt e)              = FnSqrt        <$> f e
onCallA f (FnAtan e1 e2)          = FnAtan        <$> f e1 <*> f e2
onCallA f (FnMax e1 e2)           = FnMax         <$> f e1 <*> f e2
onCallA f (FnMin e1 e2)           = FnMin         <$> f e1 <*> f e2
onCallA f (FnMod e1 e2)           = FnMod         <$> f e1 <*> f e2
onCallA f (FnPow e1 e2)           = FnPow         <$> f e1 <*> f e2
onCallA f (FnSmoothstep e1 e2 e3) = FnSmoothstep  <$> f e1 <*> f e2 <*> f e3
onCallA f (FnLengthV2 e)          = FnLengthV2    <$> f e
onCallA f (FnLengthV3 e)          = FnLengthV3    <$> f e
onCallA f (FnNormalizeV2 e)       = FnNormalizeV2 <$> f e
onCallA f (FnNormalizeV3 e)       = FnNormalizeV3 <$> f e
onCallA f (FnDotV2 e1 e2)         = FnDotV2       <$> f e1 <*> f e2
onCallA f (FnDotV3 e1 e2)         = FnDotV3       <$> f e1 <*> f e2
onCallA f (FnDotC e1 e2)          = FnDotC        <$> f e1 <*> f e2
onCallA f (FnReflectV2 e1 e2)     = FnReflectV2   <$> f e1 <*> f e2
onCallA f (FnReflectV3 e1 e2)     = FnReflectV3   <$> f e1 <*> f e2


type Traversal = {
  onBool    :: Expr Boolean -> Expr Boolean,
  onNum     :: Expr Number  -> Expr Number,
  onVec2    :: Expr Vec2    -> Expr Vec2,
  onVec3    :: Expr Vec3    -> Expr Vec3,
  onComplex :: Expr Complex -> Expr Complex,
  onColor   :: Expr Color   -> Expr Color
}

fromGeneric :: (forall a. Expr a -> Expr a) -> Traversal
fromGeneric f = {
  onBool:    f,
  onNum:     f,
  onVec2:    f,
  onVec3:    f,
  onComplex: f,
  onColor:   f
}

-- | Apply a traversal to the largest subexpression with a compatible, inferrable type
over :: forall a. Traversal -> Expr a -> Expr a
over t (EVar n)           = EVar n
over t e@(EBool b)        = eraseType $ t.onBool $ asBoolean e
over t e@(ENum n)         = eraseType $ t.onNum $ asNumber e
over t e@(EVec2 x y)      = eraseType $ t.onVec2 $ asVec2 e
over t e@(EVec3 x y z)    = eraseType $ t.onVec3 $ asVec3 e
over t e@(EComplex r i)   = eraseType $ t.onComplex $ asComplex e
over t e@(EColor r g b)   = eraseType $ t.onColor $ asColor e
over t (EUnary e)         = dispatchUnary t e
over t (EBinary e)        = dispatchBinary t e
over t (ECall e)          = dispatchCall t e
over t (EUnit)            = EUnit
over t (ETuple a b)       = ETuple (over t a) (over t b)
over t (EFst tup)         = EFst (over t tup)
over t (ESnd tup)         = ESnd (over t tup)
over t (EIf i thn els)    = EIf (t.onBool i) (over t thn) (over t els)
over t (EInl val)         = EInl (over t val)
over t (EInr val)         = EInr (over t val)
over t (EMatch e lname l rname r)  = EMatch (over t e) lname (over t l) rname (over t r)
over t (ERec n name e1 e2)         = ERec n name (over t e1) (over t e2)
over t (EBind name e1 e2)          = EBind name (over t e1) (over t e2)

dispatchUnary :: forall a. Traversal -> UnaryExpr a -> Expr a
dispatchUnary t e@(UnNegate _)        = eraseType $ t.onNum      $ asNumber  (EUnary e)
dispatchUnary t e@(UnNot _)           = eraseType $ t.onBool     $ asBoolean (EUnary e)
dispatchUnary t e@(UnProjV2X _)       = eraseType $ t.onVec2     $ asVec2    (EUnary e)
dispatchUnary t e@(UnProjV2Y _)       = eraseType $ t.onVec2     $ asVec2    (EUnary e)
dispatchUnary t e@(UnProjV3X _)       = eraseType $ t.onVec3     $ asVec3    (EUnary e)
dispatchUnary t e@(UnProjV3Y _)       = eraseType $ t.onVec3     $ asVec3    (EUnary e)
dispatchUnary t e@(UnProjV3Z _)       = eraseType $ t.onVec3     $ asVec3    (EUnary e)
dispatchUnary t e@(UnProjReal _)      = eraseType $ t.onComplex  $ asComplex (EUnary e)
dispatchUnary t e@(UnProjImaginary _) = eraseType $ t.onComplex  $ asComplex (EUnary e)
dispatchUnary t e@(UnProjR _)         = eraseType $ t.onColor    $ asColor   (EUnary e)
dispatchUnary t e@(UnProjG _)         = eraseType $ t.onColor    $ asColor   (EUnary e)
dispatchUnary t e@(UnProjB _)         = eraseType $ t.onColor    $ asColor   (EUnary e)

dispatchBinary :: forall a. Traversal -> BinaryExpr a -> Expr a
dispatchBinary t e@(BinEq _ _)       = eraseType $ t.onBool     $ asBoolean  (EBinary e)
dispatchBinary t e@(BinNeq _ _)      = eraseType $ t.onBool     $ asBoolean  (EBinary e)
dispatchBinary t e@(BinLt _ _)       = eraseType $ t.onBool     $ asBoolean  (EBinary e)
dispatchBinary t e@(BinLte _ _)      = eraseType $ t.onBool     $ asBoolean  (EBinary e)
dispatchBinary t e@(BinGt _ _)       = eraseType $ t.onBool     $ asBoolean  (EBinary e)
dispatchBinary t e@(BinGte _ _)      = eraseType $ t.onBool     $ asBoolean  (EBinary e)
dispatchBinary t e@(BinAnd _ _)      = eraseType $ t.onBool     $ asBoolean  (EBinary e)
dispatchBinary t e@(BinOr _ _)       = eraseType $ t.onBool     $ asBoolean  (EBinary e)
dispatchBinary t e@(BinPlus _ _)     = eraseType $ t.onNum      $ asNumber   (EBinary e)
dispatchBinary t e@(BinMinus _ _)    = eraseType $ t.onNum      $ asNumber   (EBinary e)
dispatchBinary t e@(BinTimes _ _)    = eraseType $ t.onNum      $ asNumber   (EBinary e)
dispatchBinary t e@(BinDiv _ _)      = eraseType $ t.onNum      $ asNumber   (EBinary e)
dispatchBinary t e@(BinPlusV2 _ _)   = eraseType $ t.onVec2     $ asVec2     (EBinary e)
dispatchBinary t e@(BinMinusV2 _ _)  = eraseType $ t.onVec2     $ asVec2     (EBinary e)
dispatchBinary t e@(BinScaleV2 _ _)  = eraseType $ t.onVec2     $ asVec2     (EBinary e)
dispatchBinary t e@(BinPlusV3 _ _)   = eraseType $ t.onVec3     $ asVec3     (EBinary e)
dispatchBinary t e@(BinMinusV3 _ _)  = eraseType $ t.onVec3     $ asVec3     (EBinary e)
dispatchBinary t e@(BinScaleV3 _ _)  = eraseType $ t.onVec3     $ asVec3     (EBinary e)
dispatchBinary t e@(BinPlusC _ _)    = eraseType $ t.onComplex  $ asComplex  (EBinary e)
dispatchBinary t e@(BinMinusC _ _)   = eraseType $ t.onComplex  $ asComplex  (EBinary e)
dispatchBinary t e@(BinTimesC _ _)   = eraseType $ t.onComplex  $ asComplex  (EBinary e)
dispatchBinary t e@(BinDivC _ _)     = eraseType $ t.onComplex  $ asComplex  (EBinary e)
dispatchBinary t e@(BinScaleC _ _)   = eraseType $ t.onComplex  $ asComplex  (EBinary e)
dispatchBinary t e@(BinPlusCol _ _)  = eraseType $ t.onColor    $ asColor    (EBinary e)
dispatchBinary t e@(BinMinusCol _ _) = eraseType $ t.onColor    $ asColor    (EBinary e)
dispatchBinary t e@(BinTimesCol _ _) = eraseType $ t.onColor    $ asColor    (EBinary e)
dispatchBinary t e@(BinScaleCol _ _) = eraseType $ t.onColor    $ asColor    (EBinary e)

dispatchCall :: forall a. Traversal -> CallExpr a -> Expr a
dispatchCall t e@(FnAbs _)            = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnCos _)            = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnFloor _)          = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnFract _)          = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnLog _)            = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnLog2 _)           = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnSaturate _)       = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnSin _)            = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnSqrt _)           = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnAtan _ _)         = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnMax _ _)          = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnMin _ _)          = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnMod _ _)          = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnPow _ _)          = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnSmoothstep _ _ _) = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnLengthV2 _)       = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnLengthV3 _)       = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnNormalizeV2 _)    = eraseType $ t.onVec2 $ asVec2   (ECall e)
dispatchCall t e@(FnNormalizeV3 _)    = eraseType $ t.onVec3 $ asVec3   (ECall e)
dispatchCall t e@(FnDotV2 _ _)        = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnDotV3 _ _)        = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnDotC _ _)         = eraseType $ t.onNum  $ asNumber (ECall e)
dispatchCall t e@(FnReflectV2 _ _)    = eraseType $ t.onVec2 $ asVec2   (ECall e)
dispatchCall t e@(FnReflectV3 _ _)    = eraseType $ t.onVec3 $ asVec3   (ECall e)


-- | Apply a function to all vars in an expression tree and accumulate the result as a monoid
foldVars :: forall a b. Monoid b => (String -> b) -> Expr a -> b
foldVars f (EVar n) = f n
foldVars f e        = foldExpr (foldVars f) e

-- | Lift a function over one level of an expression tree, and accumulate the result as a monoid
foldExpr :: forall a b. Monoid b => (forall t. Expr t -> b) -> Expr a -> b
foldExpr f (EVar n)           = mempty
foldExpr f (EBool b)          = mempty
foldExpr f (ENum n)           = mempty
foldExpr f (EVec2 x y)        = f x <> f y
foldExpr f (EVec3 x y z)      = f x <> f y <> f z
foldExpr f (EComplex r i)     = f r <> f i
foldExpr f (EColor r g b)     = f r <> f g <> f b
foldExpr f (EUnary e)         = foldExprUnary f e
foldExpr f (EBinary e)        = foldExprBinary f e
foldExpr f (ECall e)          = foldExprCall f e
foldExpr f (EUnit)            = mempty
foldExpr f (ETuple a b)       = f a <> f b
foldExpr f (EFst tup)         = f tup
foldExpr f (ESnd tup)         = f tup
foldExpr f (EIf i thn els)    = f i <> f thn <> f els
foldExpr f (EInl val)         = f val
foldExpr f (EInr val)         = f val
foldExpr f (EMatch e lname l rname r)  = f e <> f l <> f r
foldExpr f (ERec n name e1 e2)         = f e1 <> f e2
foldExpr f (EBind name e1 e2)          = f e1 <> f e2

foldExprUnary :: forall a b. Monoid b => (forall t. Expr t -> b) -> UnaryExpr a -> b
foldExprUnary f (UnNegate e)        = f e
foldExprUnary f (UnNot e)           = f e
foldExprUnary f (UnProjV2X e)       = f e
foldExprUnary f (UnProjV2Y e)       = f e
foldExprUnary f (UnProjV3X e)       = f e
foldExprUnary f (UnProjV3Y e)       = f e
foldExprUnary f (UnProjV3Z e)       = f e
foldExprUnary f (UnProjReal e)      = f e
foldExprUnary f (UnProjImaginary e) = f e
foldExprUnary f (UnProjR e)         = f e
foldExprUnary f (UnProjG e)         = f e
foldExprUnary f (UnProjB e)         = f e

foldExprBinary :: forall a b. Monoid b => (forall t. Expr t -> b) -> BinaryExpr a -> b
foldExprBinary f (BinEq e1 e2)       = f e1 <> f e2
foldExprBinary f (BinNeq e1 e2)      = f e1 <> f e2
foldExprBinary f (BinLt e1 e2)       = f e1 <> f e2
foldExprBinary f (BinLte e1 e2)      = f e1 <> f e2
foldExprBinary f (BinGt e1 e2)       = f e1 <> f e2
foldExprBinary f (BinGte e1 e2)      = f e1 <> f e2
foldExprBinary f (BinAnd e1 e2)      = f e1 <> f e2
foldExprBinary f (BinOr e1 e2)       = f e1 <> f e2
foldExprBinary f (BinPlus e1 e2)     = f e1 <> f e2
foldExprBinary f (BinMinus e1 e2)    = f e1 <> f e2
foldExprBinary f (BinTimes e1 e2)    = f e1 <> f e2
foldExprBinary f (BinDiv e1 e2)      = f e1 <> f e2
foldExprBinary f (BinPlusV2 e1 e2)   = f e1 <> f e2
foldExprBinary f (BinMinusV2 e1 e2)  = f e1 <> f e2
foldExprBinary f (BinScaleV2 e1 e2)  = f e1 <> f e2
foldExprBinary f (BinPlusV3 e1 e2)   = f e1 <> f e2
foldExprBinary f (BinMinusV3 e1 e2)  = f e1 <> f e2
foldExprBinary f (BinScaleV3 e1 e2)  = f e1 <> f e2
foldExprBinary f (BinPlusC e1 e2)    = f e1 <> f e2
foldExprBinary f (BinMinusC e1 e2)   = f e1 <> f e2
foldExprBinary f (BinTimesC e1 e2)   = f e1 <> f e2
foldExprBinary f (BinDivC e1 e2)     = f e1 <> f e2
foldExprBinary f (BinScaleC e1 e2)   = f e1 <> f e2
foldExprBinary f (BinPlusCol e1 e2)  = f e1 <> f e2
foldExprBinary f (BinMinusCol e1 e2) = f e1 <> f e2
foldExprBinary f (BinTimesCol e1 e2) = f e1 <> f e2
foldExprBinary f (BinScaleCol e1 e2) = f e1 <> f e2

foldExprCall :: forall a b. Monoid b => (forall t. Expr t -> b) -> CallExpr a -> b
foldExprCall f (FnAbs e)               = f e
foldExprCall f (FnCos e)               = f e
foldExprCall f (FnFloor e)             = f e
foldExprCall f (FnFract e)             = f e
foldExprCall f (FnLog e)               = f e
foldExprCall f (FnLog2 e)              = f e
foldExprCall f (FnSaturate e)          = f e
foldExprCall f (FnSin e)               = f e
foldExprCall f (FnSqrt e)              = f e
foldExprCall f (FnAtan e1 e2)          = f e1 <> f e2
foldExprCall f (FnMax e1 e2)           = f e1 <> f e2
foldExprCall f (FnMin e1 e2)           = f e1 <> f e2
foldExprCall f (FnMod e1 e2)           = f e1 <> f e2
foldExprCall f (FnPow e1 e2)           = f e1 <> f e2
foldExprCall f (FnSmoothstep e1 e2 e3) = f e1 <> f e2 <> f e3
foldExprCall f (FnLengthV2 e)          = f e
foldExprCall f (FnLengthV3 e)          = f e
foldExprCall f (FnNormalizeV2 e)       = f e
foldExprCall f (FnNormalizeV3 e)       = f e
foldExprCall f (FnDotV2 e1 e2)         = f e1 <> f e2
foldExprCall f (FnDotV3 e1 e2)         = f e1 <> f e2
foldExprCall f (FnDotC e1 e2)          = f e1 <> f e2
foldExprCall f (FnReflectV2 e1 e2)     = f e1 <> f e2
foldExprCall f (FnReflectV3 e1 e2)     = f e1 <> f e2
