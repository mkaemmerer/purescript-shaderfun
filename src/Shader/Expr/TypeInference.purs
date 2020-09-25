module Shader.Expr.TypeInference (inferType, Type(..)) where


import Prelude

import Data.Maybe (Maybe(..))
import Shader.Expr (BinaryExpr(..), CallExpr(..), Expr(..), UnaryExpr(..))

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

derive instance eqType :: Eq Type

inferType :: forall a. Expr a -> Maybe Type
inferType (EVar n)           = Nothing
inferType (EBool b)          = Just TBoolean
inferType (ENum n)           = Just TScalar
inferType (EVec2 x y)        = Just TVec2
inferType (EVec3 x y z)      = Just TVec3
inferType (EComplex r i)     = Just TComplex
inferType (EColor r g b)     = Just TColor
inferType (EUnary e)         = Just $ inferUnary e
inferType (EBinary e)        = Just $ inferBinary e
inferType (ECall e)          = Just $ inferCall e
inferType (EUnit)            = Just TUnit
inferType (ETuple a b)       = TTuple <$> inferType a <*> inferType b
inferType (EFst tup)         = Nothing
inferType (ESnd tup)         = Nothing
inferType (EIf i thn els)    = inferType thn
inferType (EInl val)         = Nothing
inferType (EInr val)         = Nothing
inferType (EMatch e lname l rname r)  = inferType l
inferType (ERec n name e1 e2)         = inferType e1
inferType (EBind name e1 e2)          = inferType e2

inferUnary :: forall a. UnaryExpr a -> Type
inferUnary (UnNegate e)        = TBoolean
inferUnary (UnNot e)           = TBoolean
inferUnary (UnProjV2X e)       = TScalar
inferUnary (UnProjV2Y e)       = TScalar
inferUnary (UnProjV3X e)       = TScalar
inferUnary (UnProjV3Y e)       = TScalar
inferUnary (UnProjV3Z e)       = TScalar
inferUnary (UnProjReal e)      = TScalar
inferUnary (UnProjImaginary e) = TScalar
inferUnary (UnProjR e)         = TScalar
inferUnary (UnProjG e)         = TScalar
inferUnary (UnProjB e)         = TScalar

inferBinary :: forall a. BinaryExpr a -> Type
inferBinary (BinEq e1 e2)       = TBoolean
inferBinary (BinNeq e1 e2)      = TBoolean
inferBinary (BinLt e1 e2)       = TBoolean
inferBinary (BinLte e1 e2)      = TBoolean
inferBinary (BinGt e1 e2)       = TBoolean
inferBinary (BinGte e1 e2)      = TBoolean
inferBinary (BinAnd e1 e2)      = TBoolean
inferBinary (BinOr e1 e2)       = TBoolean
inferBinary (BinPlus e1 e2)     = TScalar
inferBinary (BinMinus e1 e2)    = TScalar
inferBinary (BinTimes e1 e2)    = TScalar
inferBinary (BinDiv e1 e2)      = TScalar
inferBinary (BinPlusV2 e1 e2)   = TVec2
inferBinary (BinMinusV2 e1 e2)  = TVec2
inferBinary (BinScaleV2 e1 e2)  = TVec2
inferBinary (BinPlusV3 e1 e2)   = TVec3
inferBinary (BinMinusV3 e1 e2)  = TVec3
inferBinary (BinScaleV3 e1 e2)  = TVec3
inferBinary (BinPlusC e1 e2)    = TComplex
inferBinary (BinMinusC e1 e2)   = TComplex
inferBinary (BinTimesC e1 e2)   = TComplex
inferBinary (BinDivC e1 e2)     = TComplex
inferBinary (BinScaleC e1 e2)   = TComplex
inferBinary (BinPlusCol e1 e2)  = TColor
inferBinary (BinMinusCol e1 e2) = TColor
inferBinary (BinTimesCol e1 e2) = TColor
inferBinary (BinScaleCol e1 e2) = TColor

inferCall :: forall a. CallExpr a -> Type
inferCall (FnAbs e)               = TScalar
inferCall (FnCos e)               = TScalar
inferCall (FnFloor e)             = TScalar
inferCall (FnFract e)             = TScalar
inferCall (FnLog e)               = TScalar
inferCall (FnLog2 e)              = TScalar
inferCall (FnSaturate e)          = TScalar
inferCall (FnSin e)               = TScalar
inferCall (FnSqrt e)              = TScalar
inferCall (FnAtan e1 e2)          = TScalar
inferCall (FnMax e1 e2)           = TScalar
inferCall (FnMin e1 e2)           = TScalar
inferCall (FnMod e1 e2)           = TScalar
inferCall (FnPow e1 e2)           = TScalar
inferCall (FnSmoothstep e1 e2 e3) = TScalar
inferCall (FnLengthV2 e)          = TScalar
inferCall (FnLengthV3 e)          = TScalar
inferCall (FnNormalizeV2 e)       = TVec2
inferCall (FnNormalizeV3 e)       = TVec3
inferCall (FnDotV2 e1 e2)         = TScalar
inferCall (FnDotV3 e1 e2)         = TScalar
inferCall (FnDotC e1 e2)          = TScalar
inferCall (FnReflectV2 e1 e2)     = TVec2
inferCall (FnReflectV3 e1 e2)     = TVec3
