module Shader.Expr.Optimization
  ( class ConstantFold
  , toConst
  , cFold
  , constantFold
  , class IdentityFold
  , iFoldL
  , iFoldR
  , identityFold
  , class Annihilates
  , annihilateL
  , annihilateR
  , optimize
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Color (Color(..))
import Data.Complex (Complex(..))
import Data.HeytingAlgebra (ff, tt)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Data.Vec2 (Vec2(..))
import Data.Vec3 (Vec3(..))
import Data.VectorSpace (magnitude, zeroV, (*^), (<.>), (^+^), (^-^))
import Math (atan2, cos, log, log2e, pow, sin, sqrt)
import Shader.Expr (BinaryExpr(..), CallExpr(..), Expr(..), UnaryExpr(..))
import Shader.Expr.Cast (class Castable, cast)
import Shader.Expr.Normalization (subst)
import Shader.Expr.Traversal (Traversal, on, over)
import Unsafe.Coerce (unsafeCoerce)

eraseType :: forall a b. Expr a -> Expr b
eraseType = unsafeCoerce

-- TODO: apply these rules recursively?
-- Annihilation could create more opportunities for constant folding
optimize :: forall a. (ConstantFold a) => (IdentityFold a) => (Annihilates a) => Expr a -> Expr a
optimize = constantFold >>> identityFold >>> annihilate


-------------------------------------------------------------------------------
-- Constant Folding
-------------------------------------------------------------------------------

constantFold :: forall a. ConstantFold a => Expr a -> Expr a
constantFold e = case folded of
  Just ex -> ex
  Nothing -> on (over (constantFoldTraversal unit)) e
  where
    folded = cFold e

constantFoldTraversal :: Unit -> Traversal
constantFoldTraversal _ = {
  onBool:    constantFold,
  onNum:     constantFold,
  onVec2:    constantFold,
  onVec3:    constantFold,
  onComplex: constantFold,
  onColor:   constantFold
}

class ConstantFold t where
  -- | If the expression can be constant folded completely, returns (Just val) where val is a
  -- | purescript value (not an expression). Otherwise returns Nothing
  toConst :: Expr t -> Maybe t
  -- | Constant fold the expression if possible
  cFold :: Expr t -> Maybe (Expr t)

instance constantFoldUnit :: ConstantFold Unit where
  toConst EUnit = Just unit
  toConst e = cFold e >>= toConst
  cFold e = Nothing

instance constantFoldBoolean :: ConstantFold Boolean where
  toConst (EBool b) = Just b
  toConst e = cFold e >>= toConst
  cFold (EUnary unary) = cFold' unary
    where
    cFold' (UnNot e) = liftC1 not e
    cFold' e = Nothing
  cFold (EBinary binary) = cFold' binary
    where
    cFold' (BinEq   e1 e2) = liftC2 (==) e1 e2
    cFold' (BinNeq  e1 e2) = liftC2 (/=) e1 e2
    cFold' (BinLt   e1 e2) = liftC2 (<)  e1 e2
    cFold' (BinLte  e1 e2) = liftC2 (<=) e1 e2
    cFold' (BinGt   e1 e2) = liftC2 (>)  e1 e2
    cFold' (BinGte  e1 e2) = liftC2 (>=) e1 e2
    cFold' (BinAnd  e1 e2) = liftC2 (&&) e1 e2
    cFold' (BinOr   e1 e2) = liftC2 (||) e1 e2
    cFold' e = Nothing
  cFold e = Nothing

instance constantFoldNumber :: ConstantFold Number where
  toConst (ENum n) = Just n
  toConst e = cFold e >>= toConst
  cFold (EUnary unary) = cFold' unary
    where
    cFold' (UnNegate         e) = liftC1 negate e
    cFold' (UnProjV2X        e) = liftC1 (\(Vec2 v) -> v.x)     e
    cFold' (UnProjV2Y        e) = liftC1 (\(Vec2 v) -> v.y)     e
    cFold' (UnProjV3X        e) = liftC1 (\(Vec3 v) -> v.x)     e
    cFold' (UnProjV3Y        e) = liftC1 (\(Vec3 v) -> v.y)     e
    cFold' (UnProjV3Z        e) = liftC1 (\(Vec3 v) -> v.z)     e
    cFold' (UnProjReal       e) = liftC1 (\(Complex r i) -> r)  e
    cFold' (UnProjImaginary  e) = liftC1 (\(Complex r i) -> i)  e
    cFold' (UnProjR          e) = liftC1 (\(Color r g b) -> r)  e
    cFold' (UnProjG          e) = liftC1 (\(Color r g b) -> g)  e
    cFold' (UnProjB          e) = liftC1 (\(Color r g b) -> b)  e
    cFold' e = Nothing
  cFold (EBinary binary) = cFold' binary
    where
    cFold' (BinPlus  e1 e2) = liftC2 (+) e1 e2
    cFold' (BinMinus e1 e2) = liftC2 (-) e1 e2
    cFold' (BinTimes e1 e2) = liftC2 (*) e1 e2
    cFold' (BinDiv   e1 e2) = liftC2 (/) e1 e2
    cFold' e = Nothing
  cFold (ECall call) = cFold' call
    where
    cFold' (FnAbs e)                  = liftC1 abs e
    cFold' (FnCos e)                  = liftC1 cos e
    cFold' (FnFloor e)                = liftC1 (floor >>> toNumber) e
    cFold' (FnFract e)                = liftC1 (\x -> x `mod` 1.0) e
    cFold' (FnLog e)                  = liftC1 log e
    cFold' (FnLog2 e)                 = liftC1 (\x -> log x / log2e) e
    cFold' (FnSaturate e)             = liftC1 (clamp 0.0 1.0) e
    cFold' (FnSin e)                  = liftC1 sin e
    cFold' (FnSqrt e)                 = liftC1 sqrt e
    cFold' (FnAtan e1 e2)             = liftC2 atan2 e1 e2
    cFold' (FnMax e1 e2)              = liftC2 max e1 e2
    cFold' (FnMin e1 e2)              = liftC2 min e1 e2
    cFold' (FnMod e1 e2)              = liftC2 mod e1 e2
    cFold' (FnPow e1 e2)              = liftC2 pow e1 e2
    -- cFold' (FnSmoothstep e1 e2 e3) = TODO
    cFold' (FnLengthV2 e)             = liftC1 magnitude e
    cFold' (FnLengthV3 e)             = liftC1 magnitude e
    cFold' (FnDotV2 e1 e2)            = liftC2 (<.>) e1 e2
    cFold' (FnDotV3 e1 e2)            = liftC2 (<.>) e1 e2
    cFold' (FnDotC e1 e2)             = liftC2 (<.>) e1 e2
    -- cFold' (FnReflectV2 e1 e2)        = TODO
    -- cFold' (FnReflectV3 e1 e2)        = TODO
    cFold' e = Nothing
  cFold e = Nothing

instance constantFoldVec2 :: ConstantFold Vec2 where
  toConst (EVec2 xx yy) = (\x y -> Vec2 {x,y}) <$> (toConst xx) <*> (toConst yy)
  toConst e = cFold e >>= toConst
  cFold (EBinary binary) = cFold' binary
    where
    cFold' (BinPlusV2   e1 e2) = liftC2 (^+^) e1 e2
    cFold' (BinMinusV2  e1 e2) = liftC2 (^-^) e1 e2
    cFold' (BinScaleV2  e1 e2) = liftC2 (*^) e1 e2
    cFold' e = Nothing
  cFold e = Nothing

instance constantFoldVec3 :: ConstantFold Vec3 where
  toConst (EVec3 xx yy zz) = (\x y z -> Vec3 {x,y,z}) <$> (toConst xx) <*> (toConst yy) <*> (toConst zz)
  toConst e = cFold e >>= toConst
  cFold (EBinary binary) = cFold' binary
    where
    cFold' (BinPlusV3   e1 e2) = liftC2 (^+^) e1 e2
    cFold' (BinMinusV3  e1 e2) = liftC2 (^-^) e1 e2
    cFold' (BinScaleV3  e1 e2) = liftC2 (*^) e1 e2
    cFold' e = Nothing
  cFold e = Nothing

instance constantFoldComplex :: ConstantFold Complex where
  toConst (EComplex r i) = Complex <$> (toConst r) <*> (toConst i)
  toConst e = cFold e >>= toConst
  cFold (EBinary binary) = cFold' binary
    where
    cFold' (BinPlusC    e1 e2) = liftC2 (+) e1 e2
    cFold' (BinMinusC   e1 e2) = liftC2 (-) e1 e2
    cFold' (BinTimesC   e1 e2) = liftC2 (*) e1 e2
    cFold' (BinDivC     e1 e2) = liftC2 (/) e1 e2
    cFold' (BinScaleC   e1 e2) = liftC2 (*^) e1 e2
    cFold' e = Nothing
  cFold e = Nothing

instance constantFoldColor :: ConstantFold Color where
  toConst (EColor r g b) = Color <$> (toConst r) <*> (toConst g) <*> (toConst b)
  toConst e = cFold e >>= toConst
  cFold (EBinary binary) = cFold' binary
    where
    cFold' (BinPlusCol  e1 e2) = liftC2 (+) e1 e2
    cFold' (BinMinusCol e1 e2) = liftC2 (-) e1 e2
    cFold' (BinTimesCol e1 e2) = liftC2 (*) e1 e2
    cFold' (BinScaleCol e1 e2) = liftC2 (*^) e1 e2
    cFold' e = Nothing
  cFold e = Nothing

-- TODO: is this instance even needed?
instance constantFoldTuple :: (
  Castable (Tuple a b),
  Castable a,
  Castable b,
  ConstantFold a,
  ConstantFold b
) => ConstantFold (Tuple a b) where
  toConst (ETuple a b) = Tuple <$> (toConst a) <*> (toConst b)
  toConst e = cFold e >>= toConst
  cFold e = Nothing


liftC1 :: forall a b.
  (ConstantFold a) =>
  (Castable b) =>
  (a -> b) ->
  (Expr a) ->
  Maybe (Expr b)
liftC1 op e = op <$> (toConst e) <#> cast

liftC2 :: forall a b c.
  (ConstantFold a) =>
  (ConstantFold b) =>
  (Castable c) =>
  (a -> b -> c) ->
  (Expr a) ->
  (Expr b) ->
  Maybe (Expr c)
liftC2 op e1 e2 = op <$> (toConst e1) <*> (toConst e2) <#> cast

liftC3 :: forall a b c d.
  (ConstantFold a) =>
  (ConstantFold b) =>
  (ConstantFold c) =>
  (Castable d) =>
  (a -> b -> c -> d) ->
  (Expr a) ->
  (Expr b) ->
  (Expr c) ->
  Maybe (Expr d)
liftC3 op e1 e2 e3 = op <$> (toConst e1) <*> (toConst e2) <*> (toConst e3) <#> cast


-------------------------------------------------------------------------------
-- Identity Folding
-------------------------------------------------------------------------------

identityFold :: forall a. IdentityFold a => Expr a -> Expr a
identityFold e@(EBinary bin) = case (iFoldL bin <|> iFoldR bin) of
  Just ex -> identityFold ex
  Nothing -> on (over (identityFoldTraversal unit)) e
identityFold e = on (over (identityFoldTraversal unit)) e

identityFoldTraversal :: Unit -> Traversal
identityFoldTraversal _ = {
  onBool:    identityFold,
  onNum:     identityFold,
  onVec2:    identityFold,
  onVec3:    identityFold,
  onComplex: identityFold,
  onColor:   identityFold
}

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
  iFoldL (BinScaleCol e1 e2) = if e1 == one then Just e2 else Nothing
  iFoldL _ = Nothing
  iFoldR (BinPlusCol e1 e2)  = if e2 == zeroV then Just e1 else Nothing
  iFoldR (BinMinusCol e1 e2) = if e2 == zeroV then Just e1 else Nothing
  iFoldR (BinTimesCol e1 e2) = if e2 == one then Just e1 else Nothing
  iFoldR _ = Nothing

-------------------------------------------------------------------------------
-- Annihilation
-------------------------------------------------------------------------------

annihilate :: forall a. Annihilates a => Expr a -> Expr a
annihilate (EIf i thn els)
  | i == tt   = annihilate thn
  | i == ff   = annihilate els
annihilate (EFst (ETuple e1 e2)) = annihilate e1
annihilate (ESnd (ETuple e1 e2)) = annihilate e2
annihilate (EMatch (EInl e) lname l rname r) = annihilate $ subst lname e l
annihilate (EMatch (EInr e) lname l rname r) = annihilate $ subst rname e r
annihilate e@(EBinary bin) = case (annihilateL bin <|> annihilateR bin) of
  Just ex -> annihilate ex
  Nothing -> on (over (annihilationTraversal unit)) e
annihilate e = on (over (annihilationTraversal unit)) e

annihilationTraversal :: Unit -> Traversal
annihilationTraversal _ = {
  onBool:    annihilate,
  onNum:     annihilate,
  onVec2:    annihilate,
  onVec3:    annihilate,
  onComplex: annihilate,
  onColor:   annihilate
}

class Annihilates t where
  annihilateL :: BinaryExpr t -> Maybe (Expr t)
  annihilateR :: BinaryExpr t -> Maybe (Expr t)

instance annihilatesBool :: Annihilates Boolean where
  annihilateL (BinAnd e1 e2)      = if e1 == ff then Just ff else Nothing
  annihilateL (BinOr e1 e2)       = if e1 == tt then Just tt else Nothing
  annihilateL _ = Nothing
  annihilateR (BinAnd e1 e2)      = if e2 == ff then Just ff else Nothing
  annihilateR (BinOr e1 e2)       = if e2 == tt then Just tt else Nothing
  annihilateR _ = Nothing

instance annihilatesNumber :: Annihilates Number where
  annihilateL (BinTimes e1 e2)    = if e1 == zero then Just zero else Nothing
  annihilateL _ = Nothing
  annihilateR (BinTimes e1 e2)    = if e2 == zero then Just zero else Nothing
  annihilateR _ = Nothing

instance annihilatesVec2 :: Annihilates Vec2 where
  annihilateL (BinScaleV2 e1 e2)  = if e1 == zero then Just zeroV else Nothing
  annihilateL _ = Nothing
  annihilateR _ = Nothing

instance annihilatesVec3 :: Annihilates Vec3 where
  annihilateL (BinScaleV3 e1 e2)  = if e1 == zero then Just zeroV else Nothing
  annihilateL _ = Nothing
  annihilateR _ = Nothing

instance annihilatesComplex :: Annihilates Complex where
  annihilateL (BinScaleC e1 e2)  = if e1 == zero then Just zeroV else Nothing
  annihilateL (BinTimesC e1 e2)  = if e1 == zero then Just zeroV else Nothing
  annihilateL _ = Nothing
  annihilateR (BinTimesC e1 e2)  = if e2 == zero then Just zeroV else Nothing
  annihilateR _ = Nothing

instance annihilatesColor :: Annihilates Color where
  annihilateL (BinTimesCol e1 e2) = if e1 == zero then Just zeroV else Nothing
  annihilateL (BinScaleCol e1 e2) = if e1 == zero then Just zeroV else Nothing
  annihilateL _ = Nothing
  annihilateR (BinTimesCol e1 e2) = if e2 == zero then Just zeroV else Nothing
  annihilateR _ = Nothing
