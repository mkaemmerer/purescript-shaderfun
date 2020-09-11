module Shader.Expr.Normalization (normalize, subst) where

import Prelude

import Control.Monad.Cont (Cont, cont, runCont)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Shader.Expr (BinaryExpr(..), Expr(..), Type(..), complex, fst, projImaginary, projReal, snd, tuple)
import Shader.Expr.Traversal (on, onA)
import Unsafe.Coerce (unsafeCoerce)


normalize :: forall a. Expr a -> Expr a
normalize = unsafePartial $ liftDecls >>> elaborate >>> simplify

subst :: forall a b. String -> Expr a -> Expr b -> Expr b
subst v e1 (EVar n)
  | v == n    = eraseType e1
  | otherwise = EVar n
subst v e1 e2 = on (subst v e1) e2

elaborate :: forall a. Expr a -> Expr a
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
-- Elaborate types
elaborate (EBind name ty e1 e2) = case ty of
  TBoolean              -> EBind name ty (elaborate e1) (elaborate e2)
  TScalar               -> EBind name ty (elaborate e1) (elaborate e2)
  TVec2                 -> EBind name ty (elaborate e1) (elaborate e2)
  TVec3                 -> EBind name ty (elaborate e1) (elaborate e2)
  TComplex              -> EBind name ty (elaborate e1) (elaborate e2)
  TColor                -> EBind name ty (elaborate e1) (elaborate e2)
  TUnit                 -> elaborate e2'
    where
      e2' = subst name EUnit e2
  -- TODO: how to elaborate either types
  (TEither tl tr)       -> EBind name ty (elaborate e1) (elaborate e2)
  (TTuple t_fst t_snd)  -> elaborate $
    EBind name_fst t_fst (eraseType e1_fst) $
    EBind name_snd t_snd (eraseType e1_snd) $ e2'
    where
      e1_fst   = fst e1
      e1_snd   = snd e1
      name_fst = name <> "_fst"
      name_snd = name <> "_snd"
      tup      = tuple (EVar name_fst) (EVar name_snd)
      e2'      = subst name (eraseType tup) e2
elaborate e = on elaborate e

-- TODO: not sure this fully simplifies all cases. I.e. Inl Inr?
simplify :: forall a. Expr a -> Expr a
simplify (EFst (ETuple e1 e2))                = simplify e1
simplify (ESnd (ETuple e1 e2))                = simplify e2
simplify (EFst (EIf i t e))                   = simplify $ EIf i (EFst t) (EFst e)
simplify (ESnd (EIf i t e))                   = simplify $ EIf i (ESnd t) (ESnd e)
simplify (EFst (EMatch e lname l rname r))    = simplify $ EMatch e lname (EFst l) rname (EFst r)
simplify (ESnd (EMatch e lname l rname r))    = simplify $ EMatch e lname (ESnd l) rname (ESnd r)
simplify (EMatch (EInl e) lname l rname r)    = simplify $ eraseType $ subst lname e (eraseType l)
simplify (EMatch (EInr e) lname l rname r)    = simplify $ eraseType $ subst rname e (eraseType r)
simplify (EMatch (EIf i t e) lname l rname r) = simplify $ EIf i t' e'
  where
    t' = EMatch t lname l rname r
    e' = EMatch e lname l rname r
simplify e = on simplify e


eraseType :: forall a b. Expr a -> Expr b
eraseType = unsafeCoerce

-- Hoist declarations to top level
liftDecls :: forall a. Expr a -> Expr a
liftDecls e = runCont (liftDecl e) identity

liftDecl :: forall a b. Expr a -> Cont (Expr b) (Expr a)
liftDecl (EBind name ty e1 e2) = do
  let mkBind e1' e2' = EBind name ty (eraseType e1') e2'
  e1' <- liftDecl e1
  cont \f -> mkBind e1' (runCont (liftDecl e2) f)
liftDecl (EBindRec n name ty e1 loop e2) = do
  let mkRec e1' loop' e2' = EBindRec n name ty (eraseType e1') (eraseType loop') e2'
  e1' <- liftDecl e1
  let loop' = liftDecls $ eraseType loop
  cont \f -> mkRec e1' loop' (runCont (liftDecl e2) f)
liftDecl e = onA liftDecl e
