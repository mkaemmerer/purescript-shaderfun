module Shader.GLSL.TypeInference (inferType, inferTypes, Type(..), TypeContext, emptyContext) where


import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, gets, lift, modify_, runState)
import Data.Map (Map, insert, lookup, singleton)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
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

extractFst :: Type -> Maybe Type
extractFst (TTuple a b) = Just a
extractFst _ = Nothing

extractSnd :: Type -> Maybe Type
extractSnd (TTuple a b) = Just b
extractSnd _ = Nothing


type TypeContext = Map String Type
type TypeChecker = ExceptT Unit (State TypeContext)

emptyContext :: TypeContext
emptyContext = singleton "p" TVec2

exceptFromMaybe :: forall a. Maybe a -> TypeChecker a
exceptFromMaybe (Just a) = pure a
exceptFromMaybe Nothing  = throwError unit

lookupType :: String -> TypeChecker Type
lookupType n = do
  ty <- lift $ gets (lookup n)
  exceptFromMaybe ty

resultType :: Type -> TypeChecker Type
resultType t = pure t

ascribeType :: String -> Type -> TypeChecker Unit
ascribeType n t = modify_ (insert n t)


inferTypes :: forall a. Expr a -> TypeContext -> TypeContext
inferTypes e ctx = s
  where
    Tuple r s = runState (runExceptT (inferType e)) ctx
    -- TODO: check if type inference failed and do something?


inferType :: forall a. Expr a -> TypeChecker Type
inferType (EVar n)           = lookupType n
inferType (EBool b)          = pure TBoolean
inferType (ENum n)           = pure TScalar
inferType (EVec2 x y)        = pure TVec2
inferType (EVec3 x y z)      = pure TVec3
inferType (EComplex r i)     = pure TComplex
inferType (EColor r g b)     = pure TColor
inferType (EUnary e)         = pure $ inferUnary e
inferType (EBinary e)        = pure $ inferBinary e
inferType (ECall e)          = pure $ inferCall e
inferType (EUnit)            = pure TUnit
inferType (ETuple a b)       = TTuple <$> inferType a <*> inferType b
inferType (EFst tup)         = inferType tup >>= (extractFst >>> exceptFromMaybe)
inferType (ESnd tup)         = inferType tup >>= (extractSnd >>> exceptFromMaybe)
inferType (EInl val)         = (\t -> TEither t TUnit) <$> inferType val
inferType (EInr val)         = (\t -> TEither TUnit t) <$> inferType val
inferType (EMatch e lname l rname r)  =
  do
    _ <- inferType e
    _ <- inferType l
    inferType r
inferType (EIf i thn els) =
  do
    _ <- inferType i
    _ <- inferType thn
    inferType els
inferType (ERec n name e1 e2) =
  do
    ty <- inferType e1
    ascribeType name ty
    _ <- inferType e2
    pure ty
inferType (EBind name e1 e2) =
  do
    ty <- inferType e1
    ascribeType name ty
    inferType e2

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
