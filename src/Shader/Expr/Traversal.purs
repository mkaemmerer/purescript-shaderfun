module Shader.Expr.Traversal
  ( on
  , onA
  , over
  , overTyped
  , overUnary
  , overBinary
  , overCall
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
import Shader.Expr (BinaryExpr(..), CallExpr(..), Expr(..), Type(..), UnaryExpr(..))
import Shader.Expr.Cast (asBoolean, asColor, asComplex, asNumber, asVec2, asVec3)
import Unsafe.Coerce (unsafeCoerce)

eraseType :: forall a b. Expr a -> Expr b
eraseType = unsafeCoerce


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
on f (EMatch e lname l rname r)      = EMatch (f e) lname (f l) rname (f r)
on f (EBind name ty e1 e2)           = EBind name ty (f e1) (f e2)
on f (EBindRec n name ty e1 loop e2) = EBindRec n name ty (f e1) (f loop) (f e2)

onUnary :: forall a. (forall b. Expr b -> Expr b) -> UnaryExpr a -> UnaryExpr a
onUnary f = overUnary (fromGeneric f)

onBinary :: forall a. (forall b. Expr b -> Expr b) -> BinaryExpr a -> BinaryExpr a
onBinary f = overBinary (fromGeneric f)

onCall :: forall a. (forall b. Expr b -> Expr b) -> CallExpr a -> CallExpr a
onCall f = overCall (fromGeneric f)


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
onA f (EMatch e lname l rname r)      = EMatch <$> f e <*> pure lname <*> f l <*> pure rname <*> f r
onA f (EBind name ty e1 e2)           = (unsafeCoerce EBind) <$> pure name <*> pure ty <*> f e1 <*> f e2
onA f (EBindRec n name ty e1 loop e2) = (unsafeCoerce EBindRec) <$> pure n <*> pure name <*> pure ty <*> f e1 <*> f loop <*> f e2

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
onCallA f (FnAbs e)               = FnAbs        <$> f e
onCallA f (FnCos e)               = FnCos        <$> f e
onCallA f (FnFloor e)             = FnFloor      <$> f e
onCallA f (FnFract e)             = FnFract      <$> f e
onCallA f (FnLog e)               = FnLog        <$> f e
onCallA f (FnLog2 e)              = FnLog2       <$> f e
onCallA f (FnSaturate e)          = FnSaturate   <$> f e
onCallA f (FnSin e)               = FnSin        <$> f e
onCallA f (FnSqrt e)              = FnSqrt       <$> f e
onCallA f (FnAtan e1 e2)          = FnAtan       <$> f e1 <*> f e2
onCallA f (FnMax e1 e2)           = FnMax        <$> f e1 <*> f e2
onCallA f (FnMin e1 e2)           = FnMin        <$> f e1 <*> f e2
onCallA f (FnMod e1 e2)           = FnMod        <$> f e1 <*> f e2
onCallA f (FnPow e1 e2)           = FnPow        <$> f e1 <*> f e2
onCallA f (FnSmoothstep e1 e2 e3) = FnSmoothstep <$> f e1 <*> f e2 <*> f e3
onCallA f (FnLengthV2 e)          = FnLengthV2   <$> f e
onCallA f (FnLengthV3 e)          = FnLengthV3   <$> f e
onCallA f (FnDotV2 e1 e2)         = FnDotV2      <$> f e1 <*> f e2
onCallA f (FnDotV3 e1 e2)         = FnDotV3      <$> f e1 <*> f e2
onCallA f (FnDotC e1 e2)          = FnDotC       <$> f e1 <*> f e2
onCallA f (FnReflectV2 e1 e2)     = FnReflectV2  <$> f e1 <*> f e2
onCallA f (FnReflectV3 e1 e2)     = FnReflectV3  <$> f e1 <*> f e2


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

over :: forall a. Traversal -> Expr a -> Expr a
over t (EVar n)           = EVar n
over t (EBool b)          = EBool b
over t (ENum n)           = ENum n
over t (EVec2 x y)        = EVec2 (over t x) (over t y)
over t (EVec3 x y z)      = EVec3 (over t x) (over t y) (over t z)
over t (EComplex r i)     = EComplex (over t r) (over t i)
over t (EColor r g b)     = EColor (over t r) (over t g) (over t b)
over t (EUnary e)         = EUnary $ overUnary t e
over t (EBinary e)        = EBinary $ overBinary t e
over t (ECall e)          = ECall $ overCall t e
over t (EUnit)            = EUnit
over t (ETuple a b)       = ETuple (over t a) (over t b)
over t (EFst tup)         = EFst (over t tup)
over t (ESnd tup)         = ESnd (over t tup)
over t (EIf i thn els)    = EIf (over t i) (over t thn) (over t els)
over t (EInl val)         = EInl (over t val)
over t (EInr val)         = EInr (over t val)
over t (EMatch e lname l rname r)      = EMatch (over t e) lname (over t l) rname (over t r)
over t (EBind name ty e1 e2)           = EBind name ty (over t e1) (over t e2)
over t (EBindRec n name ty e1 loop e2) = EBindRec n name ty (over t e1) (over t loop) (over t e2)

overTyped :: forall a. Traversal -> Type -> Expr a -> Expr a
overTyped t TBoolean      e = eraseType $ t.onBool    $ asBoolean e
overTyped t TScalar       e = eraseType $ t.onNum     $ asNumber e
overTyped t TVec2         e = eraseType $ t.onVec2    $ asVec2 e
overTyped t TVec3         e = eraseType $ t.onVec3    $ asVec3 e
overTyped t TComplex      e = eraseType $ t.onComplex $ asComplex e
overTyped t TColor        e = eraseType $ t.onColor   $ asColor e
overTyped t TUnit         e = over t e
overTyped t (TTuple a b)  e = over t e
overTyped t (TEither a b) e = over t e

overUnary :: forall a.
  Traversal ->
  UnaryExpr a ->
  UnaryExpr a
overUnary t (UnNegate e)        = UnNegate        (t.onNum e)
overUnary t (UnNot e)           = UnNot           (t.onBool e)
overUnary t (UnProjV2X e)       = UnProjV2X       (t.onVec2 e)
overUnary t (UnProjV2Y e)       = UnProjV2Y       (t.onVec2 e)
overUnary t (UnProjV3X e)       = UnProjV3X       (t.onVec3 e)
overUnary t (UnProjV3Y e)       = UnProjV3Y       (t.onVec3 e)
overUnary t (UnProjV3Z e)       = UnProjV3Z       (t.onVec3 e)
overUnary t (UnProjReal e)      = UnProjReal      (t.onComplex e)
overUnary t (UnProjImaginary e) = UnProjImaginary (t.onComplex e)
overUnary t (UnProjR e)         = UnProjR         (t.onColor e)
overUnary t (UnProjG e)         = UnProjG         (t.onColor e)
overUnary t (UnProjB e)         = UnProjB         (t.onColor e)

overBinary :: forall a. Traversal -> BinaryExpr a -> BinaryExpr a
overBinary t (BinEq e1 e2)       = BinEq       (t.onNum e1)     (t.onNum e2)
overBinary t (BinNeq e1 e2)      = BinNeq      (t.onNum e1)     (t.onNum e2)
overBinary t (BinLt e1 e2)       = BinLt       (t.onNum e1)     (t.onNum e2)
overBinary t (BinLte e1 e2)      = BinLte      (t.onNum e1)     (t.onNum e2)
overBinary t (BinGt e1 e2)       = BinGt       (t.onNum e1)     (t.onNum e2)
overBinary t (BinGte e1 e2)      = BinGte      (t.onNum e1)     (t.onNum e2)
overBinary t (BinAnd e1 e2)      = BinAnd      (t.onBool e1)    (t.onBool e2)
overBinary t (BinOr e1 e2)       = BinOr       (t.onBool e1)    (t.onBool e2)
overBinary t (BinPlus e1 e2)     = BinPlus     (t.onNum e1)     (t.onNum e2)
overBinary t (BinMinus e1 e2)    = BinMinus    (t.onNum e1)     (t.onNum e2)
overBinary t (BinTimes e1 e2)    = BinTimes    (t.onNum e1)     (t.onNum e2)
overBinary t (BinDiv e1 e2)      = BinDiv      (t.onNum e1)     (t.onNum e2)
overBinary t (BinPlusV2 e1 e2)   = BinPlusV2   (t.onVec2 e1)    (t.onVec2 e2)
overBinary t (BinMinusV2 e1 e2)  = BinMinusV2  (t.onVec2 e1)    (t.onVec2 e2)
overBinary t (BinScaleV2 e1 e2)  = BinScaleV2  (t.onNum e1)     (t.onVec2 e2)
overBinary t (BinPlusV3 e1 e2)   = BinPlusV3   (t.onVec3 e1)    (t.onVec3 e2)
overBinary t (BinMinusV3 e1 e2)  = BinMinusV3  (t.onVec3 e1)    (t.onVec3 e2)
overBinary t (BinScaleV3 e1 e2)  = BinScaleV3  (t.onNum e1)     (t.onVec3 e2)
overBinary t (BinPlusC e1 e2)    = BinPlusC    (t.onComplex e1) (t.onComplex e2)
overBinary t (BinMinusC e1 e2)   = BinMinusC   (t.onComplex e1) (t.onComplex e2)
overBinary t (BinTimesC e1 e2)   = BinTimesC   (t.onComplex e1) (t.onComplex e2)
overBinary t (BinDivC e1 e2)     = BinDivC     (t.onComplex e1) (t.onComplex e2)
overBinary t (BinScaleC e1 e2)   = BinScaleC   (t.onNum e1)     (t.onComplex e2)
overBinary t (BinPlusCol e1 e2)  = BinPlusCol  (t.onColor e1)   (t.onColor e2)
overBinary t (BinMinusCol e1 e2) = BinMinusCol (t.onColor e1)   (t.onColor e2)
overBinary t (BinTimesCol e1 e2) = BinTimesCol (t.onColor e1)   (t.onColor e2)
overBinary t (BinScaleCol e1 e2) = BinScaleCol (t.onNum e1)     (t.onColor e2)

overCall :: forall a. Traversal -> CallExpr a -> CallExpr a
overCall t (FnAbs e)               = FnAbs        (t.onNum e)
overCall t (FnCos e)               = FnCos        (t.onNum e)
overCall t (FnFloor e)             = FnFloor      (t.onNum e)
overCall t (FnFract e)             = FnFract      (t.onNum e)
overCall t (FnLog e)               = FnLog        (t.onNum e)
overCall t (FnLog2 e)              = FnLog2       (t.onNum e)
overCall t (FnSaturate e)          = FnSaturate   (t.onNum e)
overCall t (FnSin e)               = FnSin        (t.onNum e)
overCall t (FnSqrt e)              = FnSqrt       (t.onNum e)
overCall t (FnAtan e1 e2)          = FnAtan       (t.onNum e1) (t.onNum e2)
overCall t (FnMax e1 e2)           = FnMax        (t.onNum e1) (t.onNum e2)
overCall t (FnMin e1 e2)           = FnMin        (t.onNum e1) (t.onNum e2)
overCall t (FnMod e1 e2)           = FnMod        (t.onNum e1) (t.onNum e2)
overCall t (FnPow e1 e2)           = FnPow        (t.onNum e1) (t.onNum e2)
overCall t (FnSmoothstep e1 e2 e3) = FnSmoothstep (t.onNum e1) (t.onNum e2) (t.onNum e3)
overCall t (FnLengthV2 e)          = FnLengthV2   (t.onVec2 e)
overCall t (FnLengthV3 e)          = FnLengthV3   (t.onVec3 e)
overCall t (FnDotV2 e1 e2)         = FnDotV2      (t.onVec2 e1)    (t.onVec2 e2)
overCall t (FnDotV3 e1 e2)         = FnDotV3      (t.onVec3 e1)    (t.onVec3 e2)
overCall t (FnDotC e1 e2)          = FnDotC       (t.onComplex e1) (t.onComplex e2)
overCall t (FnReflectV2 e1 e2)     = FnReflectV2  (t.onVec2 e1)    (t.onVec2 e2)
overCall t (FnReflectV3 e1 e2)     = FnReflectV3  (t.onVec3 e1)    (t.onVec3 e2)


foldVars :: forall a b. Monoid b => (String -> b) -> Expr a -> b
foldVars f (EVar n) = f n
foldVars f e        = foldExpr (foldVars f) e

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
foldExpr f (EMatch e lname l rname r)      = f e <> f l <> f r
foldExpr f (EBind name ty e1 e2)           = f e1 <> f e2
foldExpr f (EBindRec n name ty e1 loop e2) = f e1 <> f loop <> f e2

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
foldExprCall f (FnDotV2 e1 e2)         = f e1 <> f e2
foldExprCall f (FnDotV3 e1 e2)         = f e1 <> f e2
foldExprCall f (FnDotC e1 e2)          = f e1 <> f e2
foldExprCall f (FnReflectV2 e1 e2)     = f e1 <> f e2
foldExprCall f (FnReflectV3 e1 e2)     = f e1 <> f e2