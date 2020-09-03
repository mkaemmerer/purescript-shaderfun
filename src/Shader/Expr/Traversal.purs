module Shader.Expr.Traversal
  ( over
  , overUnary
  , overBinary
  , overCall
  , overUnaryA
  , overBinaryA
  , overCallA
  , Traversal
  , fromGeneric
  , fromGenericA
  ) where


import Prelude

import Data.Color (Color)
import Data.Complex (Complex)
import Data.Vec2 (Vec2)
import Data.Vec3 (Vec3)
import Shader.Expr (BinaryExpr(..), CallExpr(..), Expr(..), UnaryExpr(..))


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
over t (EBind n ty e1 e2) = EBind n ty (over t e1) (over t e2)

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


type TraversalA f = {
  onBool    :: Applicative f => Expr Boolean -> f (Expr Boolean),
  onNum     :: Applicative f => Expr Number  -> f (Expr Number),
  onVec2    :: Applicative f => Expr Vec2    -> f (Expr Vec2),
  onVec3    :: Applicative f => Expr Vec3    -> f (Expr Vec3),
  onComplex :: Applicative f => Expr Complex -> f (Expr Complex),
  onColor   :: Applicative f => Expr Color   -> f (Expr Color)
}

fromGenericA :: forall f. Applicative f => (forall a. Expr a -> f (Expr a)) -> TraversalA f
fromGenericA f = {
  onBool:    f,
  onNum:     f,
  onVec2:    f,
  onVec3:    f,
  onComplex: f,
  onColor:   f
}

overUnaryA :: forall a f. Applicative f => TraversalA f -> UnaryExpr a -> f (UnaryExpr a)
overUnaryA t (UnNegate e)        = UnNegate        <$> t.onNum e
overUnaryA t (UnNot e)           = UnNot           <$> t.onBool e
overUnaryA t (UnProjV2X e)       = UnProjV2X       <$> t.onVec2 e
overUnaryA t (UnProjV2Y e)       = UnProjV2Y       <$> t.onVec2 e
overUnaryA t (UnProjV3X e)       = UnProjV3X       <$> t.onVec3 e
overUnaryA t (UnProjV3Y e)       = UnProjV3Y       <$> t.onVec3 e
overUnaryA t (UnProjV3Z e)       = UnProjV3Z       <$> t.onVec3 e
overUnaryA t (UnProjReal e)      = UnProjReal      <$> t.onComplex e
overUnaryA t (UnProjImaginary e) = UnProjImaginary <$> t.onComplex e
overUnaryA t (UnProjR e)         = UnProjR         <$> t.onColor e
overUnaryA t (UnProjG e)         = UnProjG         <$> t.onColor e
overUnaryA t (UnProjB e)         = UnProjB         <$> t.onColor e

overBinaryA :: forall a f. Applicative f => TraversalA f -> BinaryExpr a -> f (BinaryExpr a)
overBinaryA t (BinEq e1 e2)       = BinEq       <$> t.onNum e1      <*> t.onNum e2
overBinaryA t (BinNeq e1 e2)      = BinNeq      <$> t.onNum e1      <*> t.onNum e2
overBinaryA t (BinLt e1 e2)       = BinLt       <$> t.onNum e1      <*> t.onNum e2
overBinaryA t (BinLte e1 e2)      = BinLte      <$> t.onNum e1      <*> t.onNum e2
overBinaryA t (BinGt e1 e2)       = BinGt       <$> t.onNum e1      <*> t.onNum e2
overBinaryA t (BinGte e1 e2)      = BinGte      <$> t.onNum e1      <*> t.onNum e2
overBinaryA t (BinAnd e1 e2)      = BinAnd      <$> t.onBool e1     <*> t.onBool e2
overBinaryA t (BinOr e1 e2)       = BinOr       <$> t.onBool e1     <*> t.onBool e2
overBinaryA t (BinPlus e1 e2)     = BinPlus     <$> t.onNum e1      <*> t.onNum e2
overBinaryA t (BinMinus e1 e2)    = BinMinus    <$> t.onNum e1      <*> t.onNum e2
overBinaryA t (BinTimes e1 e2)    = BinTimes    <$> t.onNum e1      <*> t.onNum e2
overBinaryA t (BinDiv e1 e2)      = BinDiv      <$> t.onNum e1      <*> t.onNum e2
overBinaryA t (BinPlusV2 e1 e2)   = BinPlusV2   <$> t.onVec2 e1     <*> t.onVec2 e2
overBinaryA t (BinMinusV2 e1 e2)  = BinMinusV2  <$> t.onVec2 e1     <*> t.onVec2 e2
overBinaryA t (BinScaleV2 e1 e2)  = BinScaleV2  <$> t.onNum e1      <*> t.onVec2 e2
overBinaryA t (BinPlusV3 e1 e2)   = BinPlusV3   <$> t.onVec3 e1     <*> t.onVec3 e2
overBinaryA t (BinMinusV3 e1 e2)  = BinMinusV3  <$> t.onVec3 e1     <*> t.onVec3 e2
overBinaryA t (BinScaleV3 e1 e2)  = BinScaleV3  <$> t.onNum e1      <*> t.onVec3 e2
overBinaryA t (BinPlusC e1 e2)    = BinPlusC    <$> t.onComplex e1  <*> t.onComplex e2
overBinaryA t (BinMinusC e1 e2)   = BinMinusC   <$> t.onComplex e1  <*> t.onComplex e2
overBinaryA t (BinTimesC e1 e2)   = BinTimesC   <$> t.onComplex e1  <*> t.onComplex e2
overBinaryA t (BinDivC e1 e2)     = BinDivC     <$> t.onComplex e1  <*> t.onComplex e2
overBinaryA t (BinScaleC e1 e2)   = BinScaleC   <$> t.onNum e1      <*> t.onComplex e2
overBinaryA t (BinPlusCol e1 e2)  = BinPlusCol  <$> t.onColor e1    <*> t.onColor e2
overBinaryA t (BinMinusCol e1 e2) = BinMinusCol <$> t.onColor e1    <*> t.onColor e2
overBinaryA t (BinTimesCol e1 e2) = BinTimesCol <$> t.onColor e1    <*> t.onColor e2
overBinaryA t (BinScaleCol e1 e2) = BinScaleCol <$> t.onNum e1      <*> t.onColor e2

overCallA :: forall a f. Applicative f => TraversalA f -> CallExpr a -> f (CallExpr a)
overCallA t (FnAbs e)               = FnAbs        <$> t.onNum e
overCallA t (FnCos e)               = FnCos        <$> t.onNum e
overCallA t (FnFloor e)             = FnFloor      <$> t.onNum e
overCallA t (FnFract e)             = FnFract      <$> t.onNum e
overCallA t (FnLog e)               = FnLog        <$> t.onNum e
overCallA t (FnLog2 e)              = FnLog2       <$> t.onNum e
overCallA t (FnSaturate e)          = FnSaturate   <$> t.onNum e
overCallA t (FnSin e)               = FnSin        <$> t.onNum e
overCallA t (FnSqrt e)              = FnSqrt       <$> t.onNum e
overCallA t (FnAtan e1 e2)          = FnAtan       <$> t.onNum e1 <*> t.onNum e2
overCallA t (FnMax e1 e2)           = FnMax        <$> t.onNum e1 <*> t.onNum e2
overCallA t (FnMin e1 e2)           = FnMin        <$> t.onNum e1 <*> t.onNum e2
overCallA t (FnMod e1 e2)           = FnMod        <$> t.onNum e1 <*> t.onNum e2
overCallA t (FnPow e1 e2)           = FnPow        <$> t.onNum e1 <*> t.onNum e2
overCallA t (FnSmoothstep e1 e2 e3) = FnSmoothstep <$> t.onNum e1 <*> t.onNum e2 <*> t.onNum e3
overCallA t (FnLengthV2 e)          = FnLengthV2   <$> t.onVec2 e
overCallA t (FnLengthV3 e)          = FnLengthV3   <$> t.onVec3 e
overCallA t (FnDotV2 e1 e2)         = FnDotV2      <$> t.onVec2 e1    <*> t.onVec2 e2
overCallA t (FnDotV3 e1 e2)         = FnDotV3      <$> t.onVec3 e1    <*> t.onVec3 e2
overCallA t (FnDotC e1 e2)          = FnDotC       <$> t.onComplex e1 <*> t.onComplex e2
overCallA t (FnReflectV2 e1 e2)     = FnReflectV2  <$> t.onVec2 e1    <*> t.onVec2 e2
overCallA t (FnReflectV3 e1 e2)     = FnReflectV3  <$> t.onVec3 e1    <*> t.onVec3 e2
