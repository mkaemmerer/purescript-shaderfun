module Shader.Expr.Untyped (UntypedExpr(..), mkUntypedExpr) where

import Prelude

import Data.Hash (Hash, mkHash, runHash)
import Data.Hashable (class Hashable, hash)
import Data.Newtype (class Newtype, unwrap, wrap)
import Shader.Expr (BinaryExpr(..), CallExpr(..), Expr(..), UnaryExpr(..))
import Shader.Expr.Traversal (foldExpr)
import Unsafe.Coerce (unsafeCoerce)


newtype UntypedExpr = UntypedExpr (Expr Unit)
derive instance newtypeUntypedExpr :: Newtype UntypedExpr _
derive instance eqUntypedExpr :: Eq UntypedExpr

instance hashableUntypedExpr :: Hashable UntypedExpr where
  hash = (unwrap >>> hashE >>> runHash)

mkUntypedExpr :: forall a. Expr a -> UntypedExpr
mkUntypedExpr e = wrap $ unsafeCoerce e

hashE :: forall a. Expr a -> Hash
hashE e = hashExpr e <> foldExpr hashSubexpr e
  where
    hashSubexpr :: forall b. Expr b -> Hash
    hashSubexpr e2 = mkHash $ hash $ mkUntypedExpr e2

hashExpr :: forall a. Expr a -> Hash
hashExpr (EVar _)           = mkHash 0
hashExpr (EBool _)          = mkHash 1
hashExpr (ENum _)           = mkHash 2
hashExpr (EVec2 _ _)        = mkHash 3
hashExpr (EVec3 _ _ _)      = mkHash 4
hashExpr (EComplex _ _)     = mkHash 5
hashExpr (EColor _ _ _)     = mkHash 6
hashExpr (EUnary e)         = mkHash 7 <> hashExprUnary e
hashExpr (EBinary e)        = mkHash 8 <> hashExprBinary e
hashExpr (ECall e)          = mkHash 9 <> hashExprCall e
hashExpr (EUnit)            = mkHash 10
hashExpr (ETuple _ _)       = mkHash 11
hashExpr (EFst _)           = mkHash 12
hashExpr (ESnd _)           = mkHash 13
hashExpr (EIf _ _ _)        = mkHash 14
hashExpr (EInl _)           = mkHash 15
hashExpr (EInr _)           = mkHash 16
hashExpr (EMatch _ _ _ _ _) = mkHash 17
hashExpr (ERec _ _ _ _)     = mkHash 18
hashExpr (EBind _ _ _)      = mkHash 19

hashExprUnary :: forall a. UnaryExpr a -> Hash
hashExprUnary (UnNegate _)        = mkHash 0
hashExprUnary (UnNot _)           = mkHash 1
hashExprUnary (UnProjV2X _)       = mkHash 2
hashExprUnary (UnProjV2Y _)       = mkHash 3
hashExprUnary (UnProjV3X _)       = mkHash 4
hashExprUnary (UnProjV3Y _)       = mkHash 5
hashExprUnary (UnProjV3Z _)       = mkHash 6
hashExprUnary (UnProjReal _)      = mkHash 7
hashExprUnary (UnProjImaginary _) = mkHash 8
hashExprUnary (UnProjR _)         = mkHash 9
hashExprUnary (UnProjG _)         = mkHash 10
hashExprUnary (UnProjB _)         = mkHash 11

hashExprBinary :: forall a. BinaryExpr a -> Hash
hashExprBinary (BinEq _ _ )       = mkHash 0
hashExprBinary (BinNeq _ _ )      = mkHash 1
hashExprBinary (BinLt _ _ )       = mkHash 2
hashExprBinary (BinLte _ _ )      = mkHash 3
hashExprBinary (BinGt _ _ )       = mkHash 4
hashExprBinary (BinGte _ _ )      = mkHash 5
hashExprBinary (BinAnd _ _ )      = mkHash 6
hashExprBinary (BinOr _ _ )       = mkHash 7
hashExprBinary (BinPlus _ _ )     = mkHash 8
hashExprBinary (BinMinus _ _ )    = mkHash 9
hashExprBinary (BinTimes _ _ )    = mkHash 10
hashExprBinary (BinDiv _ _ )      = mkHash 11
hashExprBinary (BinPlusV2 _ _ )   = mkHash 12
hashExprBinary (BinMinusV2 _ _ )  = mkHash 13
hashExprBinary (BinScaleV2 _ _ )  = mkHash 14
hashExprBinary (BinPlusV3 _ _ )   = mkHash 15
hashExprBinary (BinMinusV3 _ _ )  = mkHash 16
hashExprBinary (BinScaleV3 _ _ )  = mkHash 17
hashExprBinary (BinPlusC _ _ )    = mkHash 18
hashExprBinary (BinMinusC _ _ )   = mkHash 19
hashExprBinary (BinTimesC _ _ )   = mkHash 20
hashExprBinary (BinDivC _ _ )     = mkHash 21
hashExprBinary (BinScaleC _ _ )   = mkHash 22
hashExprBinary (BinPlusCol _ _ )  = mkHash 23
hashExprBinary (BinMinusCol _ _ ) = mkHash 24
hashExprBinary (BinTimesCol _ _ ) = mkHash 25
hashExprBinary (BinScaleCol _ _ ) = mkHash 26

hashExprCall :: forall a. CallExpr a -> Hash
hashExprCall (FnAbs _)             = mkHash 0
hashExprCall (FnCos _)             = mkHash 1
hashExprCall (FnFloor _)           = mkHash 2
hashExprCall (FnFract _)           = mkHash 3
hashExprCall (FnLog _)             = mkHash 4
hashExprCall (FnLog2 _)            = mkHash 5
hashExprCall (FnSaturate _)        = mkHash 6
hashExprCall (FnSin _)             = mkHash 7
hashExprCall (FnSqrt _)            = mkHash 8
hashExprCall (FnAtan _ _)          = mkHash 9
hashExprCall (FnMax _ _)           = mkHash 10
hashExprCall (FnMin _ _)           = mkHash 11
hashExprCall (FnMod _ _)           = mkHash 12
hashExprCall (FnPow _ _)           = mkHash 13
hashExprCall (FnSmoothstep _ _ _)  = mkHash 14
hashExprCall (FnLengthV2 _)        = mkHash 15
hashExprCall (FnLengthV3 _)        = mkHash 16
hashExprCall (FnNormalizeV2 _)     = mkHash 17
hashExprCall (FnNormalizeV3 _)     = mkHash 18
hashExprCall (FnDotV2 _ _)         = mkHash 19
hashExprCall (FnDotV3 _ _)         = mkHash 20
hashExprCall (FnDotC _ _)          = mkHash 21
hashExprCall (FnReflectV2 _ _)     = mkHash 22
hashExprCall (FnReflectV3 _ _)     = mkHash 23
