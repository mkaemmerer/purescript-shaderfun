module Shader.Expr.Optimization (class ConstantFold, toConst, cFold, constantFold, class IdentityFold, iFoldL, iFoldR, identityFold) where

import Prelude

import Control.Alt ((<|>))
import Data.Color (Color(..))
import Data.Complex (Complex(..))
import Data.HeytingAlgebra (ff, tt)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (abs)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Vec2 (Vec2(..))
import Data.Vec3 (Vec3(..))
import Data.VectorSpace (magnitude, zeroV, (*^), (<.>), (^+^), (^-^))
import Math (atan2, cos, log, log2e, pow, sin, sqrt)
import Shader.Expr (BinaryExpr(..), CallExpr(..), Expr(..), Type(..), UnaryExpr(..))
import Shader.Expr.Cast (class Castable, cast)
import Shader.Expr.Traversal (Traversal, over, overBinary, overCall, overTyped, overUnary)
import Unsafe.Coerce (unsafeCoerce)

eraseType :: forall a b. Expr a -> Expr b
eraseType = unsafeCoerce

-------------------------------------------------------------------------------
-- Constant Folding
-------------------------------------------------------------------------------

constantFold :: forall a. ConstantFold a => Expr a -> Expr a
constantFold (EBind name ty e1 e2) = EBind name ty (eraseType e1') e2'
  where
    e1' = constantFoldWithType ty e1
    e2' = constantFold e2
constantFold (EBindRec n name ty e1 loop e2) = EBindRec n name ty (eraseType e1') (eraseType loop') e2'
  where
    e1'   = constantFoldWithType ty e1
    loop' = constantFoldWithType (TEither ty ty) loop
    e2'   = constantFold e2
constantFold (EUnary e)  = cFoldWithDefault (EUnary $ constantFoldUnary e)
constantFold (EBinary e) = cFoldWithDefault (EBinary $ constantFoldBinary e)
constantFold (ECall e)   = cFoldWithDefault (ECall $ constantFoldCall e)
constantFold (EIf i t e) = EIf (constantFold i) (constantFold t) (constantFold e)
constantFold e = cFoldWithDefault e

constantFoldWithType :: forall a. Type -> Expr a -> Expr a
constantFoldWithType ty e = overTyped (constantFoldTraversal unit) ty e

cFoldWithDefault :: forall a. ConstantFold a => Expr a -> Expr a
cFoldWithDefault e = maybe e identity (cFold e)

constantFoldTraversal :: Unit -> Traversal
constantFoldTraversal _ = {
  onBool: constantFold,
  onNum: constantFold,
  onVec2: constantFold,
  onVec3: constantFold,
  onComplex: constantFold,
  onColor: constantFold
}

constantFoldUnary :: forall a. ConstantFold a => UnaryExpr a -> UnaryExpr a
constantFoldUnary = overUnary $ constantFoldTraversal unit

constantFoldBinary :: forall a. ConstantFold a => BinaryExpr a -> BinaryExpr a
constantFoldBinary = overBinary $ constantFoldTraversal unit

constantFoldCall :: forall a. ConstantFold a => CallExpr a -> CallExpr a
constantFoldCall = overCall $ constantFoldTraversal unit

class ConstantFold t where
  toConst :: Expr t -> Maybe t
  cFold :: Expr t -> Maybe (Expr t)

instance constantFoldUnit :: ConstantFold Unit where
  toConst EUnit = Just unit
  toConst _ = Nothing
  cFold e = cFoldDefault e

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
    cFold' (FnPow e1 e2)              = lift2 pow e1 e2
    -- cFold' (FnSmoothstep e1 e2 e3) = TODO
    cFold' (FnLengthV2 e)             = lift1 magnitude e
    cFold' (FnLengthV3 e)             = lift1 magnitude e
    cFold' (FnDotV2 e1 e2)            = lift2 (<.>) e1 e2
    cFold' (FnDotV3 e1 e2)            = lift2 (<.>) e1 e2
    cFold' (FnDotC e1 e2)             = lift2 (<.>) e1 e2
    -- cFold' (FnReflectV2 e1 e2)        = TODO
    -- cFold' (FnReflectV3 e1 e2)        = TODO
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


-------------------------------------------------------------------------------
-- Identity Folding
-------------------------------------------------------------------------------

identityFold :: forall a. IdentityFold a => Expr a -> Expr a
identityFold (EBind name ty e1 e2) = EBind name ty (eraseType e1') e2'
  where
    e1' = identityFoldWithType ty e1
    e2' = identityFold e2
identityFold (EBindRec n name ty e1 loop e2) = EBindRec n name ty (eraseType e1') (eraseType loop') e2'
  where
    e1'   = identityFoldWithType ty e1
    loop' = identityFoldWithType (TEither ty ty) loop
    e2'   = identityFold e2
identityFold (EBinary e) = iFoldWithDefault (EBinary e') e'
  where
    e' = identityFoldBinary e
    iFold ex = iFoldL ex <|> iFoldR ex
    iFoldWithDefault d ex = maybe d identity (iFold ex)
identityFold e = over (identityFoldTraversal unit) e

identityFoldWithType :: forall a. Type -> Expr a -> Expr a
identityFoldWithType ty e = overTyped (identityFoldTraversal unit) ty e

identityFoldTraversal :: Unit -> Traversal
identityFoldTraversal _ = {
  onBool: identityFold,
  onNum: identityFold,
  onVec2: identityFold,
  onVec3: identityFold,
  onComplex: identityFold,
  onColor: identityFold
}

identityFoldUnary :: forall a. IdentityFold a => UnaryExpr a -> UnaryExpr a
identityFoldUnary = overUnary $ identityFoldTraversal unit

identityFoldBinary :: forall a. IdentityFold a => BinaryExpr a -> BinaryExpr a
identityFoldBinary = overBinary $ identityFoldTraversal unit

identityFoldCall :: forall a. IdentityFold a => CallExpr a -> CallExpr a
identityFoldCall = overCall $ identityFoldTraversal unit


class IdentityFold t where
  iFoldL :: BinaryExpr t -> Maybe (Expr t)
  iFoldR :: BinaryExpr t -> Maybe (Expr t)

instance identityFoldUnit :: IdentityFold Unit where
  iFoldL _ = Nothing
  iFoldR _ = Nothing

instance identityFoldBool :: IdentityFold Boolean where
  iFoldL (BinAnd e1 e2)      = if e1 == tt then Just e2 else Nothing
  iFoldL (BinOr e1 e2)       = if e1 == ff then Just e2 else Nothing
  iFoldL _ = Nothing
  iFoldR (BinAnd e1 e2)      = if e2 == tt then Just e1 else Nothing
  iFoldR (BinOr e1 e2)       = if e2 == ff then Just e1 else Nothing
  iFoldR _ = Nothing

instance identityFoldNumber :: IdentityFold Number where
  iFoldL (BinPlus e1 e2)     = if e1 == zero then Just e2 else Nothing
  iFoldL (BinTimes e1 e2)    = if e1 == one then Just e2 else Nothing
  iFoldL _ = Nothing
  iFoldR (BinPlus e1 e2)     = if e2 == zero then Just e1 else Nothing
  iFoldR (BinMinus e1 e2)    = if e2 == zero then Just e1 else Nothing
  iFoldR (BinTimes e1 e2)    = if e2 == one then Just e1 else Nothing
  iFoldR (BinDiv e1 e2)      = if e2 == one then Just e1 else Nothing
  iFoldR _ = Nothing

instance identityFoldVec2 :: IdentityFold Vec2 where
  iFoldL (BinPlusV2 e1 e2)   = if e1 == zeroV then Just e2 else Nothing
  iFoldL (BinScaleV2 e1 e2)  = if e1 == one then Just e2 else Nothing
  iFoldL _ = Nothing
  iFoldR (BinPlusV2 e1 e2)   = if e2 == zeroV then Just e1 else Nothing
  iFoldR (BinMinusV2 e1 e2)  = if e2 == zeroV then Just e1 else Nothing
  iFoldR _ = Nothing

instance identityFoldVec3 :: IdentityFold Vec3 where
  iFoldL (BinPlusV3 e1 e2)   = if e1 == zeroV then Just e2 else Nothing
  iFoldL (BinScaleV3 e1 e2)  = if e1 == one then Just e2 else Nothing
  iFoldL _ = Nothing
  iFoldR (BinPlusV3 e1 e2)   = if e2 == zeroV then Just e1 else Nothing
  iFoldR (BinMinusV3 e1 e2)  = if e2 == zeroV then Just e1 else Nothing
  iFoldR _ = Nothing

instance identityFoldComplex :: IdentityFold Complex where
  iFoldL (BinPlusC e1 e2)    = if e1 == zero then Just e2 else Nothing
  iFoldL (BinTimesC e1 e2)   = if e1 == one then Just e2 else Nothing
  iFoldL (BinScaleC e1 e2)   = if e1 == one then Just e2 else Nothing
  iFoldL _ = Nothing
  iFoldR (BinPlusC e1 e2)    = if e2 == zero then Just e1 else Nothing
  iFoldR (BinMinusC e1 e2)   = if e2 == zero then Just e1 else Nothing
  iFoldR (BinTimesC e1 e2)   = if e2 == one then Just e1 else Nothing
  iFoldR (BinDivC e1 e2)     = if e2 == one then Just e1 else Nothing
  iFoldR _ = Nothing

instance identityFoldColor :: IdentityFold Color where
  iFoldL (BinPlusCol e1 e2)  = if e1 == zeroV then Just e2 else Nothing
  iFoldL (BinTimesCol e1 e2) = if e1 == zeroV then Just e2 else Nothing
  iFoldL (BinScaleCol e1 e2) = if e1 == zeroV then Just e2 else Nothing
  iFoldL _ = Nothing
  iFoldR (BinPlusCol e1 e2)  = if e2 == zeroV then Just e1 else Nothing
  iFoldR (BinMinusCol e1 e2) = if e2 == zeroV then Just e1 else Nothing
  iFoldR (BinTimesCol e1 e2) = if e2 == one then Just e1 else Nothing
  iFoldR _ = Nothing
