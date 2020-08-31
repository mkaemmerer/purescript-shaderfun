module Shader.Expr.Normalization (normalize) where

import Prelude

import Control.Monad.Cont (Cont, runCont, callCC)
import Data.Tuple (Tuple(..))
import Partial (crash)
import Partial.Unsafe (unsafePartial)
import Shader.Expr (class TypedExpr, BinaryExpr(..), CallExpr(..), Expr(..), UnaryExpr(..), complex, projImaginary, projReal)
import Unsafe.Coerce (unsafeCoerce)


normalize :: forall a. TypedExpr a => Expr a -> Expr a
normalize = unsafePartial $ elaborate >>> liftDecls


elaborate :: Partial => forall a. Expr a -> Expr a
elaborate (EBinary (BinTimesC c1 c2)) = eraseType $ complex (r1*r2 - i1*i2) (r1*i2 + r2*i1)
  where
  c1' = elaborate c1
  c2' = elaborate c2
  Tuple r1 i1 = Tuple (projReal c1') (projImaginary c1')
  Tuple r2 i2 = Tuple (projReal c2') (projImaginary c2')
elaborate (EBinary (BinDivC c1 c2)) = eraseType $ complex nr ni
  where
  c1' = elaborate c1
  c2' = elaborate c2
  Tuple r1 i1 = Tuple (projReal c1') (projImaginary c1')
  Tuple r2 i2 = Tuple (projReal c2') (projImaginary c2')
  nr = (r1*r2 + i1*i2) / d
  ni = (r2*i1 - r1*i2) / d
  d  = r2*r2 + i2*i2
elaborate (EFst (ETuple e1 e2)) = elaborate e1
elaborate (ESnd (ETuple e1 e2)) = elaborate e2
elaborate (EFst (EIf c t e)) = EIf (elaborate c) (elaborate $ EFst t) (elaborate $ EFst e)
elaborate (ESnd (EIf c t e)) = EIf (elaborate c) (elaborate $ ESnd t) (elaborate $ ESnd e)
-- Other cases
elaborate (EVar name)              = EVar name
elaborate (ENum n)                 = ENum n
elaborate (EBool b)                = EBool b
elaborate (EVec2 x y)              = EVec2 (elaborate x) (elaborate y)
elaborate (EVec3 x y z)            = EVec3 (elaborate x) (elaborate y) (elaborate z)
elaborate (EComplex r i)           = EComplex (elaborate r) (elaborate i)
elaborate (EColor r g b)           = EColor (elaborate r) (elaborate g) (elaborate b)
elaborate (EUnary e)               = EUnary (elaborateUnary e)
elaborate (EBinary e)              = EBinary (elaborateBinary e)
elaborate (ECall e)                = ECall (elaborateCall e)
elaborate (EIf i t e)              = EIf (elaborate i) (elaborate t) (elaborate e)
elaborate (EBind name ty val body) = EBind name ty (elaborate val) (elaborate body)
-- Not handled
elaborate (EFst _) = crash     -- ETuple/EIf are the only inhabitants of type (Expr (Tuple a b))
elaborate (ESnd _) = crash     -- ETuple/EIf are the only inhabitants of type (Expr (Tuple a b))
elaborate (ETuple _ _) = crash -- ETuple is not a valid top-level term
elaborate (EUnit) = crash      -- Unit is not a valid top-level term

elaborateUnary :: Partial => forall a. UnaryExpr a -> UnaryExpr a
elaborateUnary (UnNegate e)        = UnNegate (elaborate e)
elaborateUnary (UnNot e)           = UnNot (elaborate e)
elaborateUnary (UnProjV2X e)       = UnProjV2X (elaborate e)
elaborateUnary (UnProjV2Y e)       = UnProjV2Y (elaborate e)
elaborateUnary (UnProjV3X e)       = UnProjV3X (elaborate e)
elaborateUnary (UnProjV3Y e)       = UnProjV3Y (elaborate e)
elaborateUnary (UnProjV3Z e)       = UnProjV3Z (elaborate e)
elaborateUnary (UnProjR e)         = UnProjR (elaborate e)
elaborateUnary (UnProjG e)         = UnProjG (elaborate e)
elaborateUnary (UnProjB e)         = UnProjB (elaborate e)
elaborateUnary (UnProjReal e)      = UnProjReal (elaborate e)
elaborateUnary (UnProjImaginary e) = UnProjImaginary (elaborate e)

elaborateBinary :: Partial => forall a. BinaryExpr a -> BinaryExpr a
elaborateBinary (BinTimes l r)     = BinTimes (elaborate l) (elaborate r)
elaborateBinary (BinDiv l r)       = BinDiv (elaborate l) (elaborate r)
elaborateBinary (BinScaleV2 l r)   = BinScaleV2 (elaborate l) (elaborate r)
elaborateBinary (BinScaleV3 l r)   = BinScaleV3 (elaborate l) (elaborate r)
elaborateBinary (BinScaleC l r)    = BinScaleC (elaborate l) (elaborate r)
elaborateBinary (BinTimesC l r)    = BinTimesC (elaborate l) (elaborate r)
elaborateBinary (BinDivC l r)      = BinDivC (elaborate l) (elaborate r)
elaborateBinary (BinScaleCol l r)  = BinScaleCol (elaborate l) (elaborate r)
elaborateBinary (BinTimesCol l r)  = BinTimesCol (elaborate l) (elaborate r)
elaborateBinary (BinPlus l r)      = BinPlus (elaborate l) (elaborate r)
elaborateBinary (BinPlusV2 l r)    = BinPlusV2 (elaborate l) (elaborate r)
elaborateBinary (BinPlusV3 l r)    = BinPlusV3 (elaborate l) (elaborate r)
elaborateBinary (BinPlusC l r)     = BinPlusC (elaborate l) (elaborate r)
elaborateBinary (BinPlusCol l r)   = BinPlusCol (elaborate l) (elaborate r)
elaborateBinary (BinMinus l r)     = BinMinus (elaborate l) (elaborate r)
elaborateBinary (BinMinusV2 l r)   = BinMinusV2 (elaborate l) (elaborate r)
elaborateBinary (BinMinusV3 l r)   = BinMinusV3 (elaborate l) (elaborate r)
elaborateBinary (BinMinusC l r)    = BinMinusC (elaborate l) (elaborate r)
elaborateBinary (BinMinusCol l r)  = BinMinusCol (elaborate l) (elaborate r)
elaborateBinary (BinLt l r)        = BinLt (elaborate l) (elaborate r)
elaborateBinary (BinLte l r)       = BinLte (elaborate l) (elaborate r)
elaborateBinary (BinGt l r)        = BinGt (elaborate l) (elaborate r)
elaborateBinary (BinGte l r)       = BinGte (elaborate l) (elaborate r)
elaborateBinary (BinEq l r)        = BinEq (elaborate l) (elaborate r)
elaborateBinary (BinNeq l r)       = BinNeq (elaborate l) (elaborate r)
elaborateBinary (BinAnd l r)       = BinAnd (elaborate l) (elaborate r)
elaborateBinary (BinOr l r)        = BinOr (elaborate l) (elaborate r)

elaborateCall :: Partial => forall a. CallExpr a -> CallExpr a
elaborateCall (FnAbs e)               = FnAbs (elaborate e)
elaborateCall (FnCos e)               = FnCos (elaborate e)
elaborateCall (FnFloor e)             = FnFloor (elaborate e)
elaborateCall (FnFract e)             = FnFract (elaborate e)
elaborateCall (FnLog e)               = FnLog (elaborate e)
elaborateCall (FnLog2 e)              = FnLog2 (elaborate e)
elaborateCall (FnSaturate e)          = FnSaturate (elaborate e)
elaborateCall (FnSin e)               = FnSin (elaborate e)
elaborateCall (FnSqrt e)              = FnSqrt (elaborate e)
elaborateCall (FnAtan e1 e2)          = FnAtan (elaborate e1) (elaborate e2)
elaborateCall (FnMax e1 e2)           = FnMax (elaborate e1) (elaborate e2)
elaborateCall (FnMin e1 e2)           = FnMin (elaborate e1) (elaborate e2)
elaborateCall (FnMod e1 e2)           = FnMod (elaborate e1) (elaborate e2)
elaborateCall (FnSmoothstep e1 e2 e3) = FnSmoothstep (elaborate e1) (elaborate e2) (elaborate e3)
elaborateCall (FnLengthV2 e)          = FnLengthV2 (elaborate e)
elaborateCall (FnLengthV3 e)          = FnLengthV3 (elaborate e)
elaborateCall (FnDotV2 e1 e2)         = FnDotV2 (elaborate e1) (elaborate e2)
elaborateCall (FnDotV3 e1 e2)         = FnDotV3 (elaborate e1) (elaborate e2)
elaborateCall (FnDotC e1 e2)          = FnDotC (elaborate e1) (elaborate e2)
elaborateCall (FnReflectV2 e1 e2)     = FnReflectV2 (elaborate e1) (elaborate e2)
elaborateCall (FnReflectV3 e1 e2)     = FnReflectV3 (elaborate e1) (elaborate e2)
elaborateCall (FnMix e1 e2 e3)        = FnMix (elaborate e1) (elaborate e2) (elaborate e3)

eraseType :: forall a b. Expr a -> Expr b
eraseType = unsafeCoerce

-- Hoist declarations to top level
liftDecls :: Partial => forall a. Expr a -> Expr a
liftDecls e = runCont (liftDecl e) identity

liftDecl :: Partial => forall a b. Expr a -> Cont (Expr b) (Expr a)
liftDecl (EVar name) = callCC $ (#) $ EVar name
liftDecl (EBool b) = callCC $ (#) $ EBool b
liftDecl (ENum n) = callCC $ (#) $ ENum n
liftDecl (EVec2 x y) = do
  x' <- liftDecl x
  y' <- liftDecl y
  callCC $ (#) $ EVec2 x' y'
liftDecl (EVec3 x y z) = do
  x' <- liftDecl x
  y' <- liftDecl y
  z' <- liftDecl z
  callCC $ (#) $ EVec3 x' y' z'
liftDecl (EComplex r i) = do
  r' <- liftDecl r
  i' <- liftDecl i
  callCC $ (#) $ EComplex r' i'
liftDecl (EColor r g b) = do
  r' <- liftDecl r
  g' <- liftDecl g
  b' <- liftDecl b
  callCC $ (#) $ EColor r' g' b'
liftDecl (EUnary e) = do
  e' <- liftDeclUnary e
  callCC $ (#) $ EUnary e'
liftDecl (EBinary e) = do
  e' <- liftDeclBinary e
  callCC $ (#) $ EBinary e'
liftDecl (ECall e) = do
  e' <- liftDeclCall e
  callCC $ (#) $ ECall e
liftDecl (EIf i t e) = do
  i' <- liftDecl i
  t' <- liftDecl t
  e' <- liftDecl e
  callCC $ (#) $ EIf i' t' e'
liftDecl (EBind name ty val body) = do
  body' <- liftDecl body
  pure $ EBind name ty val body'
-- Not handled
liftDecl (ETuple _ _) = crash
liftDecl (EFst _) = crash
liftDecl (ESnd _) = crash
liftDecl (EUnit) = crash

liftDeclUnary :: Partial => forall a b. UnaryExpr a -> Cont (Expr b) (UnaryExpr a)
liftDeclUnary (UnNegate e)        = UnNegate        <$> liftDecl e
liftDeclUnary (UnNot e)           = UnNot           <$> liftDecl e
liftDeclUnary (UnProjV2X e)       = UnProjV2X       <$> liftDecl e
liftDeclUnary (UnProjV2Y e)       = UnProjV2Y       <$> liftDecl e
liftDeclUnary (UnProjV3X e)       = UnProjV3X       <$> liftDecl e
liftDeclUnary (UnProjV3Y e)       = UnProjV3Y       <$> liftDecl e
liftDeclUnary (UnProjV3Z e)       = UnProjV3Z       <$> liftDecl e
liftDeclUnary (UnProjR e)         = UnProjR         <$> liftDecl e
liftDeclUnary (UnProjG e)         = UnProjG         <$> liftDecl e
liftDeclUnary (UnProjB e)         = UnProjB         <$> liftDecl e
liftDeclUnary (UnProjReal e)      = UnProjReal      <$> liftDecl e
liftDeclUnary (UnProjImaginary e) = UnProjImaginary <$> liftDecl e

liftDeclBinary :: Partial => forall a b. BinaryExpr a -> Cont (Expr b) (BinaryExpr a)
liftDeclBinary (BinTimes l r)     = BinTimes    <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinDiv l r)       = BinDiv      <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinScaleV2 l r)   = BinScaleV2  <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinScaleV3 l r)   = BinScaleV3  <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinScaleC l r)    = BinScaleC   <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinTimesC l r)    = BinTimesC   <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinDivC l r)      = BinDivC     <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinScaleCol l r)  = BinScaleCol <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinTimesCol l r)  = BinTimesCol <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinPlus l r)      = BinPlus     <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinPlusV2 l r)    = BinPlusV2   <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinPlusV3 l r)    = BinPlusV3   <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinPlusC l r)     = BinPlusC    <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinPlusCol l r)   = BinPlusCol  <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinMinus l r)     = BinMinus    <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinMinusV2 l r)   = BinMinusV2  <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinMinusV3 l r)   = BinMinusV3  <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinMinusC l r)    = BinMinusC   <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinMinusCol l r)  = BinMinusCol <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinLt l r)        = BinLt       <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinLte l r)       = BinLte      <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinGt l r)        = BinGt       <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinGte l r)       = BinGte      <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinEq l r)        = BinEq       <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinNeq l r)       = BinNeq      <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinAnd l r)       = BinAnd      <$> liftDecl l <*> liftDecl r
liftDeclBinary (BinOr l r)        = BinOr       <$> liftDecl l <*> liftDecl r

liftDeclCall :: Partial => forall a b. CallExpr a -> Cont (Expr b) (CallExpr a)
liftDeclCall (FnAbs e)               = FnAbs        <$> liftDecl e
liftDeclCall (FnCos e)               = FnCos        <$> liftDecl e
liftDeclCall (FnFloor e)             = FnFloor      <$> liftDecl e
liftDeclCall (FnFract e)             = FnFract      <$> liftDecl e
liftDeclCall (FnLog e)               = FnLog        <$> liftDecl e
liftDeclCall (FnLog2 e)              = FnLog2       <$> liftDecl e
liftDeclCall (FnSaturate e)          = FnSaturate   <$> liftDecl e
liftDeclCall (FnSin e)               = FnSin        <$> liftDecl e
liftDeclCall (FnSqrt e)              = FnSqrt       <$> liftDecl e
liftDeclCall (FnAtan e1 e2)          = FnAtan       <$> liftDecl e1 <*> liftDecl e2
liftDeclCall (FnMax e1 e2)           = FnMax        <$> liftDecl e1 <*> liftDecl e2
liftDeclCall (FnMin e1 e2)           = FnMin        <$> liftDecl e1 <*> liftDecl e2
liftDeclCall (FnMod e1 e2)           = FnMod        <$> liftDecl e1 <*> liftDecl e2
liftDeclCall (FnSmoothstep e1 e2 e3) = FnSmoothstep <$> liftDecl e1 <*> liftDecl e2 <*> liftDecl e3
liftDeclCall (FnLengthV2 e)          = FnLengthV2   <$> liftDecl e
liftDeclCall (FnLengthV3 e)          = FnLengthV3   <$> liftDecl e
liftDeclCall (FnDotV2 e1 e2)         = FnDotV2      <$> liftDecl e1 <*> liftDecl e2
liftDeclCall (FnDotV3 e1 e2)         = FnDotV3      <$> liftDecl e1 <*> liftDecl e2
liftDeclCall (FnDotC e1 e2)          = FnDotC       <$> liftDecl e1 <*> liftDecl e2
liftDeclCall (FnReflectV2 e1 e2)     = FnReflectV2  <$> liftDecl e1 <*> liftDecl e2
liftDeclCall (FnReflectV3 e1 e2)     = FnReflectV3  <$> liftDecl e1 <*> liftDecl e2
liftDeclCall (FnMix e1 e2 e3)        = FnMix        <$> liftDecl e1 <*> liftDecl e2 <*> liftDecl e3
