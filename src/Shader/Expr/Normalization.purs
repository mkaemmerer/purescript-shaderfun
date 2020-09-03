module Shader.Expr.Normalization (normalize) where

import Prelude

import Control.Monad.Cont (Cont, runCont, callCC)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Shader.Expr (class TypedExpr, BinaryExpr(..), CallExpr, Expr(..), Type(..), UnaryExpr, complex, fst, projImaginary, projReal, snd, tuple)
import Shader.Expr.Cast (asBoolean, asColor, asComplex, asNumber, asVec2, asVec3)
import Shader.Expr.Traversal (fromGeneric, fromGenericA, over, overBinaryA, overCallA, overUnaryA)
import Unsafe.Coerce (unsafeCoerce)


subst :: forall a. String -> Expr a -> Expr a -> Expr a
subst v e1 (EVar n)
  | v == n    = e1
  | otherwise = EVar n
subst v e1 e2 = over traversal e2
  where
    traversal = {
      onBool:    subst v (asBoolean e1),
      onNum:     subst v (asNumber e1),
      onVec2:    subst v (asVec2 e1),
      onVec3:    subst v (asVec3 e1),
      onComplex: subst v (asComplex e1),
      onColor:   subst v (asColor e1)
    }


normalize :: forall a. TypedExpr a => Expr a -> Expr a
normalize = unsafePartial $ liftDecls >>> elaborate


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
elaborate (EFst (EIf i t e)) = EIf (elaborate i) (elaborate $ EFst t) (elaborate $ EFst e)
elaborate (ESnd (EIf i t e)) = EIf (elaborate i) (elaborate $ ESnd t) (elaborate $ ESnd e)
elaborate (EIf i t e)        = EIf (elaborate i) (elaborate t) (elaborate e)
-- Elaborate types
elaborate (EBind name ty val body) = case ty of
  TBoolean              -> EBind name ty (elaborate val) (elaborate body)
  TScalar               -> EBind name ty (elaborate val) (elaborate body)
  TVec2                 -> EBind name ty (elaborate val) (elaborate body)
  TVec3                 -> EBind name ty (elaborate val) (elaborate body)
  TComplex              -> EBind name ty (elaborate val) (elaborate body)
  TColor                -> EBind name ty (elaborate val) (elaborate body)
  (TTuple t_fst t_snd)  -> elaborate $
    EBind name_fst t_fst (eraseType val_fst) $
    EBind name_snd t_snd (eraseType val_snd) $ body'
    where
      val_fst  = fst val
      val_snd  = snd val
      name_fst = name <> "_fst"
      name_snd = name <> "_snd"
      tup      = tuple val_fst val_snd
      body'    = subst name (eraseType tup) (elaborate body)
elaborate e = over (fromGeneric elaborate) e


eraseType :: forall a b. Expr a -> Expr b
eraseType = unsafeCoerce

-- Hoist declarations to top level
writeDecl :: forall a b. Cont (Expr b) (Expr a) -> Cont (Expr b) (Expr a)
writeDecl e = e >>= \v -> callCC (v # _)

liftDecls :: Partial => forall a. Expr a -> Expr a
liftDecls e = runCont (liftDecl e) identity

liftDecl :: Partial => forall a b. Expr a -> Cont (Expr b) (Expr a)
liftDecl (EVar name)    = writeDecl $ pure (EVar name)
liftDecl (EBool b)      = writeDecl $ pure (EBool b)
liftDecl (ENum n)       = writeDecl $ pure (ENum n)
liftDecl (EVec2 x y)    = writeDecl $ EVec2    <$> liftDecl x <*> liftDecl y
liftDecl (EVec3 x y z)  = writeDecl $ EVec3    <$> liftDecl x <*> liftDecl y <*> liftDecl z
liftDecl (EComplex r i) = writeDecl $ EComplex <$> liftDecl r <*> liftDecl i
liftDecl (EColor r g b) = writeDecl $ EColor   <$> liftDecl r <*> liftDecl g <*> liftDecl b
liftDecl (EUnary e)     = writeDecl $ EUnary   <$> liftDeclUnary e
liftDecl (EBinary e)    = writeDecl $ EBinary  <$> liftDeclBinary e
liftDecl (ECall e)      = writeDecl $ ECall    <$> liftDeclCall e
liftDecl (EIf i t e)    = writeDecl $ EIf      <$> liftDecl i <*> liftDecl t <*> liftDecl e
liftDecl (EFst e)       = writeDecl $ EFst <$> liftDecl e
liftDecl (ESnd e)       = writeDecl $ ESnd <$> liftDecl e
liftDecl (EUnit)        = writeDecl $ pure EUnit
liftDecl (EBind name ty val body) = (EBind name ty val) <$> liftDecl body
liftDecl (ETuple e1 e2) = writeDecl $ (unsafeCoerce ETuple) <$> liftDecl e1 <*> liftDecl e2

liftDeclUnary :: Partial => forall a b. UnaryExpr a -> Cont (Expr b) (UnaryExpr a)
liftDeclUnary = overUnaryA $ fromGenericA liftDecl

liftDeclBinary :: Partial => forall a b. BinaryExpr a -> Cont (Expr b) (BinaryExpr a)
liftDeclBinary = overBinaryA $ fromGenericA liftDecl

liftDeclCall :: Partial => forall a b. CallExpr a -> Cont (Expr b) (CallExpr a)
liftDeclCall = overCallA $ fromGenericA liftDecl