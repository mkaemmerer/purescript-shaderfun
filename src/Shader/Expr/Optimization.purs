module Shader.Expr.Optimization (class ConstantFold, toConst, cFold, constantFold) where

import Prelude

import Data.Color (Color(..))
import Data.Complex (Complex(..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (abs)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Vec2 (Vec2(..))
import Data.Vec3 (Vec3(..))
import Data.VectorSpace (magnitude, (*^), (^+^), (^-^), (<.>))
import Math (atan2, cos, log, log2e, sin, sqrt)
import Shader.Expr (BinaryExpr(..), CallExpr(..), Expr(..), Type(..), UnaryExpr(..))
import Shader.Expr.Cast (class Castable, asBoolean, asColor, asComplex, asNumber, asVec2, asVec3, cast)
import Unsafe.Coerce (unsafeCoerce)

foldWithDefault :: forall a. ConstantFold a => Expr a -> Expr a
foldWithDefault e = maybe e identity (cFold e)

constantFold :: forall a. ConstantFold a => Expr a -> Expr a
constantFold (EBind name ty val body) = EBind name ty (eraseType val') body'
  where
    eraseType = unsafeCoerce
    val' = case ty of
      TBoolean -> eraseType $ constantFold $ asBoolean val
      TScalar  -> eraseType $ constantFold $ asNumber val
      TVec2    -> eraseType $ constantFold $ asVec2 val
      TVec3    -> eraseType $ constantFold $ asVec3 val
      TComplex -> eraseType $ constantFold $ asComplex val
      TColor   -> eraseType $ constantFold $ asColor val
    body' = constantFold body
constantFold (EUnary e)  = foldWithDefault (EUnary $ constantFoldUnary e)
constantFold (EBinary e) = foldWithDefault (EBinary $ constantFoldBinary e)
constantFold (ECall e)   = foldWithDefault (ECall $ constantFoldCall e)
constantFold e = foldWithDefault e

constantFoldUnary :: forall a. ConstantFold a => UnaryExpr a -> UnaryExpr a
constantFoldUnary (UnNegate e)        = UnNegate        (constantFold e)
constantFoldUnary (UnNot e)           = UnNot           (constantFold e)
constantFoldUnary (UnProjV2X e)       = UnProjV2X       (constantFold e)
constantFoldUnary (UnProjV2Y e)       = UnProjV2Y       (constantFold e)
constantFoldUnary (UnProjV3X e)       = UnProjV3X       (constantFold e)
constantFoldUnary (UnProjV3Y e)       = UnProjV3Y       (constantFold e)
constantFoldUnary (UnProjV3Z e)       = UnProjV3Z       (constantFold e)
constantFoldUnary (UnProjReal e)      = UnProjReal      (constantFold e)
constantFoldUnary (UnProjImaginary e) = UnProjImaginary (constantFold e)
constantFoldUnary (UnProjR e)         = UnProjR         (constantFold e)
constantFoldUnary (UnProjG e)         = UnProjG         (constantFold e)
constantFoldUnary (UnProjB e)         = UnProjB         (constantFold e)

constantFoldBinary :: forall a. ConstantFold a => BinaryExpr a -> BinaryExpr a
constantFoldBinary (BinEq e1 e2)       = BinEq       (constantFold e1) (constantFold e2)
constantFoldBinary (BinNeq e1 e2)      = BinNeq      (constantFold e1) (constantFold e2)
constantFoldBinary (BinLt e1 e2)       = BinLt       (constantFold e1) (constantFold e2)
constantFoldBinary (BinLte e1 e2)      = BinLte      (constantFold e1) (constantFold e2)
constantFoldBinary (BinGt e1 e2)       = BinGt       (constantFold e1) (constantFold e2)
constantFoldBinary (BinGte e1 e2)      = BinGte      (constantFold e1) (constantFold e2)
constantFoldBinary (BinAnd e1 e2)      = BinAnd      (constantFold e1) (constantFold e2)
constantFoldBinary (BinOr e1 e2)       = BinOr       (constantFold e1) (constantFold e2)
constantFoldBinary (BinPlus e1 e2)     = BinPlus     (constantFold e1) (constantFold e2)
constantFoldBinary (BinMinus e1 e2)    = BinMinus    (constantFold e1) (constantFold e2)
constantFoldBinary (BinTimes e1 e2)    = BinTimes    (constantFold e1) (constantFold e2)
constantFoldBinary (BinDiv e1 e2)      = BinDiv      (constantFold e1) (constantFold e2)
constantFoldBinary (BinPlusV2 e1 e2)   = BinPlusV2   (constantFold e1) (constantFold e2)
constantFoldBinary (BinMinusV2 e1 e2)  = BinMinusV2  (constantFold e1) (constantFold e2)
constantFoldBinary (BinScaleV2 e1 e2)  = BinScaleV2  (constantFold e1) (constantFold e2)
constantFoldBinary (BinPlusV3 e1 e2)   = BinPlusV3   (constantFold e1) (constantFold e2)
constantFoldBinary (BinMinusV3 e1 e2)  = BinMinusV3  (constantFold e1) (constantFold e2)
constantFoldBinary (BinScaleV3 e1 e2)  = BinScaleV3  (constantFold e1) (constantFold e2)
constantFoldBinary (BinPlusC e1 e2)    = BinPlusC    (constantFold e1) (constantFold e2)
constantFoldBinary (BinMinusC e1 e2)   = BinMinusC   (constantFold e1) (constantFold e2)
constantFoldBinary (BinTimesC e1 e2)   = BinTimesC   (constantFold e1) (constantFold e2)
constantFoldBinary (BinDivC e1 e2)     = BinDivC     (constantFold e1) (constantFold e2)
constantFoldBinary (BinScaleC e1 e2)   = BinScaleC   (constantFold e1) (constantFold e2)
constantFoldBinary (BinPlusCol e1 e2)  = BinPlusCol  (constantFold e1) (constantFold e2)
constantFoldBinary (BinMinusCol e1 e2) = BinMinusCol (constantFold e1) (constantFold e2)
constantFoldBinary (BinTimesCol e1 e2) = BinTimesCol (constantFold e1) (constantFold e2)
constantFoldBinary (BinScaleCol e1 e2) = BinScaleCol (constantFold e1) (constantFold e2)

constantFoldCall :: forall a. ConstantFold a => CallExpr a -> CallExpr a
constantFoldCall (FnAbs e)               = FnAbs        (constantFold e)
constantFoldCall (FnCos e)               = FnCos        (constantFold e)
constantFoldCall (FnFloor e)             = FnFloor      (constantFold e)
constantFoldCall (FnFract e)             = FnFract      (constantFold e)
constantFoldCall (FnLog e)               = FnLog        (constantFold e)
constantFoldCall (FnLog2 e)              = FnLog2       (constantFold e)
constantFoldCall (FnSaturate e)          = FnSaturate   (constantFold e)
constantFoldCall (FnSin e)               = FnSin        (constantFold e)
constantFoldCall (FnSqrt e)              = FnSqrt       (constantFold e)
constantFoldCall (FnAtan e1 e2)          = FnAtan       (constantFold e1) (constantFold e2)
constantFoldCall (FnMax e1 e2)           = FnMax        (constantFold e1) (constantFold e2)
constantFoldCall (FnMin e1 e2)           = FnMin        (constantFold e1) (constantFold e2)
constantFoldCall (FnMod e1 e2)           = FnMod        (constantFold e1) (constantFold e2)
constantFoldCall (FnSmoothstep e1 e2 e3) = FnSmoothstep (constantFold e1) (constantFold e2) (constantFold e3)
constantFoldCall (FnLengthV2 e)          = FnLengthV2   (constantFold e)
constantFoldCall (FnLengthV3 e)          = FnLengthV3   (constantFold e)
constantFoldCall (FnDotV2 e1 e2)         = FnDotV2      (constantFold e1) (constantFold e2)
constantFoldCall (FnDotV3 e1 e2)         = FnDotV3      (constantFold e1) (constantFold e2)
constantFoldCall (FnDotC e1 e2)          = FnDotC       (constantFold e1) (constantFold e2)
constantFoldCall (FnReflectV2 e1 e2)     = FnReflectV2  (constantFold e1) (constantFold e2)
constantFoldCall (FnReflectV3 e1 e2)     = FnReflectV3  (constantFold e1) (constantFold e2)
constantFoldCall (FnMix e1 e2 e3)        = FnMix        (constantFold e1) (constantFold e2) (constantFold e3)

class ConstantFold t where
  toConst :: Expr t -> Maybe t
  cFold :: Expr t -> Maybe (Expr t)

instance constantFoldBoolean :: ConstantFold Boolean where
  toConst (EBool b) = Just b
  toConst _ = Nothing
  cFold (EUnary unary) = cFold' unary
    where
    cFold' (UnNot e) = lift1 not e
    cFold' e = Nothing
  cFold (EBinary binary) = cFold' binary
    where
    cFold' (BinEq   e1 e2) = lift2 (==) e1 e2
    cFold' (BinNeq  e1 e2) = lift2 (/=) e1 e2
    cFold' (BinLt   e1 e2) = lift2 (<)  e1 e2
    cFold' (BinLte  e1 e2) = lift2 (<=) e1 e2
    cFold' (BinGt   e1 e2) = lift2 (>)  e1 e2
    cFold' (BinGte  e1 e2) = lift2 (>=) e1 e2
    cFold' (BinAnd  e1 e2) = lift2 (&&) e1 e2
    cFold' (BinOr   e1 e2) = lift2 (||) e1 e2
    cFold' e = Nothing
  cFold e = cFoldDefault e

instance constantFoldNumber :: ConstantFold Number where
  toConst (ENum n) = Just n
  toConst _ = Nothing
  cFold (EUnary unary) = cFold' unary
    where
    cFold' (UnNegate         e) = lift1 negate e
    cFold' (UnProjV2X        e) = lift1 (\(Vec2 v) -> v.x)     e
    cFold' (UnProjV2Y        e) = lift1 (\(Vec2 v) -> v.y)     e
    cFold' (UnProjV3X        e) = lift1 (\(Vec3 v) -> v.x)     e
    cFold' (UnProjV3Y        e) = lift1 (\(Vec3 v) -> v.y)     e
    cFold' (UnProjV3Z        e) = lift1 (\(Vec3 v) -> v.z)     e
    cFold' (UnProjReal       e) = lift1 (\(Complex r i) -> r)  e
    cFold' (UnProjImaginary  e) = lift1 (\(Complex r i) -> i)  e
    cFold' (UnProjR          e) = lift1 (\(Color r g b) -> r)  e
    cFold' (UnProjG          e) = lift1 (\(Color r g b) -> g)  e
    cFold' (UnProjB          e) = lift1 (\(Color r g b) -> b)  e
    cFold' e = Nothing
  cFold (EBinary binary) = cFold' binary
    where
    cFold' (BinPlus  e1 e2) = lift2 (+) e1 e2
    cFold' (BinMinus e1 e2) = lift2 (-) e1 e2
    cFold' (BinTimes e1 e2) = lift2 (*) e1 e2
    cFold' (BinDiv   e1 e2) = lift2 (/) e1 e2
    cFold' e = Nothing
  cFold (ECall call) = cFold' call
    where
    cFold' (FnAbs e)                  = lift1 abs e
    cFold' (FnCos e)                  = lift1 cos e
    cFold' (FnFloor e)                = lift1 (floor >>> toNumber) e
    cFold' (FnFract e)                = lift1 (\x -> x `mod` 1.0) e
    cFold' (FnLog e)                  = lift1 log e
    cFold' (FnLog2 e)                 = lift1 (\x -> log x / log2e) e
    cFold' (FnSaturate e)             = lift1 (clamp 0.0 1.0) e
    cFold' (FnSin e)                  = lift1 sin e
    cFold' (FnSqrt e)                 = lift1 sqrt e
    cFold' (FnAtan e1 e2)             = lift2 atan2 e1 e2
    cFold' (FnMax e1 e2)              = lift2 max e1 e2
    cFold' (FnMin e1 e2)              = lift2 min e1 e2
    cFold' (FnMod e1 e2)              = lift2 mod e1 e2
    -- cFold' (FnSmoothstep e1 e2 e3) = TODO
    cFold' (FnLengthV2 e)             = lift1 magnitude e
    cFold' (FnLengthV3 e)             = lift1 magnitude e
    cFold' (FnDotV2 e1 e2)            = lift2 (<.>) e1 e2
    cFold' (FnDotV3 e1 e2)            = lift2 (<.>) e1 e2
    cFold' (FnDotC e1 e2)             = lift2 (<.>) e1 e2
    -- cFold' (FnReflectV2 e1 e2)        = TODO
    -- cFold' (FnReflectV3 e1 e2)        = TODO
    -- cFold' (FnMix e1 e2 e3)           = TODO
    cFold' e = Nothing
  cFold e = cFoldDefault e

instance constantFoldVec2 :: ConstantFold Vec2 where
  toConst (EVec2 xx yy) = (\x y -> Vec2 {x,y}) <$> (toConst xx) <*> (toConst yy)
  toConst _ = Nothing
  cFold (EBinary binary) = cFold' binary
    where
    cFold' (BinPlusV2   e1 e2) = lift2 (^+^) e1 e2
    cFold' (BinMinusV2  e1 e2) = lift2 (^-^) e1 e2
    cFold' (BinScaleV2  e1 e2) = lift2 (*^) e1 e2
    cFold' e = Nothing
  cFold e = cFoldDefault e

instance constantFoldVec3 :: ConstantFold Vec3 where
  toConst (EVec3 xx yy zz) = (\x y z -> Vec3 {x,y,z}) <$> (toConst xx) <*> (toConst yy) <*> (toConst zz)
  toConst _ = Nothing
  cFold (EBinary binary) = cFold' binary
    where
    cFold' (BinPlusV3   e1 e2) = lift2 (^+^) e1 e2
    cFold' (BinMinusV3  e1 e2) = lift2 (^-^) e1 e2
    cFold' (BinScaleV3  e1 e2) = lift2 (*^) e1 e2
    cFold' e = Nothing
  cFold e = cFoldDefault e

instance constantFoldComplex :: ConstantFold Complex where
  toConst (EComplex r i) = Complex <$> (toConst r) <*> (toConst i)
  toConst _ = Nothing
  cFold (EBinary binary) = cFold' binary
    where
    cFold' (BinPlusC    e1 e2) = lift2 (+) e1 e2
    cFold' (BinMinusC   e1 e2) = lift2 (-) e1 e2
    cFold' (BinTimesC   e1 e2) = lift2 (*) e1 e2
    cFold' (BinDivC     e1 e2) = lift2 (/) e1 e2
    cFold' (BinScaleC   e1 e2) = lift2 (*^) e1 e2
    cFold' e = Nothing
  cFold e = cFoldDefault e

instance constantFoldColor :: ConstantFold Color where
  toConst (EColor r g b) = Color <$> (toConst r) <*> (toConst g) <*> (toConst b)
  toConst _ = Nothing
  cFold (EBinary binary) = cFold' binary
    where
    cFold' (BinPlusCol  e1 e2) = lift2 (+) e1 e2
    cFold' (BinMinusCol e1 e2) = lift2 (-) e1 e2
    cFold' (BinTimesCol e1 e2) = lift2 (*) e1 e2
    cFold' (BinScaleCol e1 e2) = lift2 (*^) e1 e2
    cFold' e = Nothing
  cFold e = cFoldDefault e

-- TODO: is this instance even needed?
instance constantFoldTuple :: (Castable (Tuple a b), Castable a, Castable b, ConstantFold a, ConstantFold b) => ConstantFold (Tuple a b) where
  toConst (ETuple a b) = Tuple <$> (toConst a) <*> (toConst b)
  toConst _ = Nothing
  cFold e = cFoldDefault e


lift1 :: forall a b.
  (ConstantFold a) =>
  (Castable b) =>
  (a -> b) ->
  (Expr a) ->
  Maybe (Expr b)
lift1 op e = op <$> (toConst e) <#> cast

lift2 :: forall a b c.
  (ConstantFold a) =>
  (ConstantFold b) =>
  (Castable c) =>
  (a -> b -> c) ->
  (Expr a) ->
  (Expr b) ->
  Maybe (Expr c)
lift2 op e1 e2 = op <$> (toConst e1) <*> (toConst e2) <#> cast

lift3 :: forall a b c d.
  (ConstantFold a) =>
  (ConstantFold b) =>
  (ConstantFold c) =>
  (Castable d) =>
  (a -> b -> c -> d) ->
  (Expr a) ->
  (Expr b) ->
  (Expr c) ->
  Maybe (Expr d)
lift3 op e1 e2 e3 = op <$> (toConst e1) <*> (toConst e2) <*> (toConst e3) <#> cast

cFoldIf :: forall a. ConstantFold a => Expr Boolean -> Expr a -> Expr a -> Maybe (Expr a)
cFoldIf i t e = do
  i' <- cFold >=> toConst $ i
  if i' then cFold t else cFold e

cFoldFst :: forall a. Castable a => ConstantFold (Tuple a Number) => Expr (forall b. Tuple a b) -> Maybe (Expr a)
cFoldFst tup = cast <$> (fst <$> toConst tup')
  where
    tup' :: Expr (Tuple a Number)
    tup' = unsafeCoerce tup

cFoldSnd :: forall b. Castable b => ConstantFold (Tuple Number b) => Expr (forall a. Tuple a b) -> Maybe (Expr b)
cFoldSnd tup = cast <$> (snd <$> toConst tup')
  where
    tup' :: Expr (Tuple Number b)
    tup' = unsafeCoerce tup

cFoldDefault :: forall a.
  Castable a =>
  ConstantFold a =>
  ConstantFold (Tuple a Number) =>
  ConstantFold (Tuple Number a) =>
  Expr a -> Maybe (Expr a)
cFoldDefault (EIf i t e) = cFoldIf i t e
cFoldDefault (EFst e) = cFoldFst e
cFoldDefault (ESnd e) = cFoldSnd e
cFoldDefault e = cast <$> toConst e
