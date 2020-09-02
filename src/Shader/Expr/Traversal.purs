module Shader.Expr.Traversal (exl, exr, ex, overUnary, overBinary, overCall, Traversal) where

import Data.Color (Color)
import Data.Complex (Complex)
import Data.Vec2 (Vec2)
import Data.Vec3 (Vec3)
import Shader.Expr (BinaryExpr(..), CallExpr(..), Expr, UnaryExpr(..))
import Unsafe.Coerce (unsafeCoerce)

eraseType :: forall a b. Expr a -> Expr b
eraseType = unsafeCoerce

exl :: forall a b. BinaryExpr a -> Expr b
exl (BinEq l r)        = eraseType l
exl (BinNeq l r)       = eraseType l
exl (BinAnd l r)       = eraseType l
exl (BinOr l r)        = eraseType l
exl (BinLt l r)        = eraseType l
exl (BinLte l r)       = eraseType l
exl (BinGt l r)        = eraseType l
exl (BinGte l r)       = eraseType l
exl (BinPlus l r)      = eraseType l
exl (BinMinus l r)     = eraseType l
exl (BinTimes l r)     = eraseType l
exl (BinDiv l r)       = eraseType l
exl (BinPlusV2 l r)    = eraseType l
exl (BinMinusV2 l r)   = eraseType l
exl (BinScaleV2 l r)   = eraseType l
exl (BinPlusV3 l r)    = eraseType l
exl (BinMinusV3 l r)   = eraseType l
exl (BinScaleV3 l r)   = eraseType l
exl (BinPlusC l r)     = eraseType l
exl (BinMinusC l r)    = eraseType l
exl (BinScaleC l r)    = eraseType l
exl (BinPlusCol l r)   = eraseType l
exl (BinMinusCol l r)  = eraseType l
exl (BinTimesCol l r)  = eraseType l
exl (BinScaleCol l r)  = eraseType l
exl (BinDivC l r)      = eraseType l
exl (BinTimesC l r)    = eraseType l

exr :: forall a b. BinaryExpr a -> Expr b
exr (BinEq l r)        = eraseType r
exr (BinNeq l r)       = eraseType r
exr (BinAnd l r)       = eraseType r
exr (BinOr l r)        = eraseType r
exr (BinLt l r)        = eraseType r
exr (BinLte l r)       = eraseType r
exr (BinGt l r)        = eraseType r
exr (BinGte l r)       = eraseType r
exr (BinPlus l r)      = eraseType r
exr (BinMinus l r)     = eraseType r
exr (BinTimes l r)     = eraseType r
exr (BinDiv l r)       = eraseType r
exr (BinPlusV2 l r)    = eraseType r
exr (BinMinusV2 l r)   = eraseType r
exr (BinScaleV2 l r)   = eraseType r
exr (BinPlusV3 l r)    = eraseType r
exr (BinMinusV3 l r)   = eraseType r
exr (BinScaleV3 l r)   = eraseType r
exr (BinPlusC l r)     = eraseType r
exr (BinMinusC l r)    = eraseType r
exr (BinScaleC l r)    = eraseType r
exr (BinPlusCol l r)   = eraseType r
exr (BinMinusCol l r)  = eraseType r
exr (BinTimesCol l r)  = eraseType r
exr (BinScaleCol l r)  = eraseType r
exr (BinDivC l r)      = eraseType r
exr (BinTimesC l r)    = eraseType r

ex :: forall a b. UnaryExpr a -> Expr b
ex (UnNegate e)        = eraseType e
ex (UnNot e)           = eraseType e
ex (UnProjV2X e)       = eraseType e
ex (UnProjV2Y e)       = eraseType e
ex (UnProjV3X e)       = eraseType e
ex (UnProjV3Y e)       = eraseType e
ex (UnProjV3Z e)       = eraseType e
ex (UnProjReal e)      = eraseType e
ex (UnProjImaginary e) = eraseType e
ex (UnProjR e)         = eraseType e
ex (UnProjG e)         = eraseType e
ex (UnProjB e)         = eraseType e

type Traversal = {
  onBool :: Expr Boolean -> Expr Boolean,
  onNum :: Expr Number -> Expr Number,
  onVec2 :: Expr Vec2 -> Expr Vec2,
  onVec3 :: Expr Vec3 -> Expr Vec3,
  onComplex :: Expr Complex -> Expr Complex,
  onColor :: Expr Color -> Expr Color
}

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
overBinary t (BinEq e1 e2)       = BinEq       (t.onNum e1) (t.onNum e2)
overBinary t (BinNeq e1 e2)      = BinNeq      (t.onNum e1) (t.onNum e2)
overBinary t (BinLt e1 e2)       = BinLt       (t.onNum e1) (t.onNum e2)
overBinary t (BinLte e1 e2)      = BinLte      (t.onNum e1) (t.onNum e2)
overBinary t (BinGt e1 e2)       = BinGt       (t.onNum e1) (t.onNum e2)
overBinary t (BinGte e1 e2)      = BinGte      (t.onNum e1) (t.onNum e2)
overBinary t (BinAnd e1 e2)      = BinAnd      (t.onBool e1) (t.onBool e2)
overBinary t (BinOr e1 e2)       = BinOr       (t.onBool e1) (t.onBool e2)
overBinary t (BinPlus e1 e2)     = BinPlus     (t.onNum e1) (t.onNum e2)
overBinary t (BinMinus e1 e2)    = BinMinus    (t.onNum e1) (t.onNum e2)
overBinary t (BinTimes e1 e2)    = BinTimes    (t.onNum e1) (t.onNum e2)
overBinary t (BinDiv e1 e2)      = BinDiv      (t.onNum e1) (t.onNum e2)
overBinary t (BinPlusV2 e1 e2)   = BinPlusV2   (t.onVec2 e1) (t.onVec2 e2)
overBinary t (BinMinusV2 e1 e2)  = BinMinusV2  (t.onVec2 e1) (t.onVec2 e2)
overBinary t (BinScaleV2 e1 e2)  = BinScaleV2  (t.onNum e1) (t.onVec2 e2)
overBinary t (BinPlusV3 e1 e2)   = BinPlusV3   (t.onVec3 e1) (t.onVec3 e2)
overBinary t (BinMinusV3 e1 e2)  = BinMinusV3  (t.onVec3 e1) (t.onVec3 e2)
overBinary t (BinScaleV3 e1 e2)  = BinScaleV3  (t.onNum e1) (t.onVec3 e2)
overBinary t (BinPlusC e1 e2)    = BinPlusC    (t.onComplex e1) (t.onComplex e2)
overBinary t (BinMinusC e1 e2)   = BinMinusC   (t.onComplex e1) (t.onComplex e2)
overBinary t (BinTimesC e1 e2)   = BinTimesC   (t.onComplex e1) (t.onComplex e2)
overBinary t (BinDivC e1 e2)     = BinDivC     (t.onComplex e1) (t.onComplex e2)
overBinary t (BinScaleC e1 e2)   = BinScaleC   (t.onNum e1) (t.onComplex e2)
overBinary t (BinPlusCol e1 e2)  = BinPlusCol  (t.onColor e1) (t.onColor e2)
overBinary t (BinMinusCol e1 e2) = BinMinusCol (t.onColor e1) (t.onColor e2)
overBinary t (BinTimesCol e1 e2) = BinTimesCol (t.onColor e1) (t.onColor e2)
overBinary t (BinScaleCol e1 e2) = BinScaleCol (t.onNum e1) (t.onColor e2)

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
overCall t (FnDotV2 e1 e2)         = FnDotV2      (t.onVec2 e1) (t.onVec2 e2)
overCall t (FnDotV3 e1 e2)         = FnDotV3      (t.onVec3 e1) (t.onVec3 e2)
overCall t (FnDotC e1 e2)          = FnDotC       (t.onComplex e1) (t.onComplex e2)
overCall t (FnReflectV2 e1 e2)     = FnReflectV2  (t.onVec2 e1) (t.onVec2 e2)
overCall t (FnReflectV3 e1 e2)     = FnReflectV3  (t.onVec3 e1) (t.onVec3 e2)
