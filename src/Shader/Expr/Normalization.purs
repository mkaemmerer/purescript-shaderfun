module Shader.Expr.Normalization (normalize) where

import Prelude

import Control.Monad.Cont (Cont, runCont, callCC)
import Data.Tuple (Tuple(..))
import Partial (crash)
import Partial.Unsafe (unsafePartial)
import Shader.Expr (class TypedExpr, BinaryExpr(..), CallExpr, Expr(..), UnaryExpr, complex, projImaginary, projReal)
import Shader.Expr.Traversal (fromGeneric, fromGenericA, overBinary, overBinaryA, overCall, overCallA, overUnary, overUnaryA)
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
elaborate (EFst _)     = crash -- ETuple/EIf are the only inhabitants of type (Expr (Tuple a b))
elaborate (ESnd _)     = crash -- ETuple/EIf are the only inhabitants of type (Expr (Tuple a b))
elaborate (ETuple _ _) = crash -- ETuple is not a valid top-level term
elaborate (EUnit)      = crash -- Unit is not a valid top-level term

elaborateUnary :: Partial => forall a. UnaryExpr a -> UnaryExpr a
elaborateUnary = overUnary $ fromGeneric elaborate

elaborateBinary :: Partial => forall a. BinaryExpr a -> BinaryExpr a
elaborateBinary = overBinary $ fromGeneric elaborate

elaborateCall :: Partial => forall a. CallExpr a -> CallExpr a
elaborateCall = overCall $ fromGeneric elaborate

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
liftDecl (EBind name ty val body) = (EBind name ty val) <$> liftDecl body
-- Not handled
liftDecl (ETuple _ _) = crash
liftDecl (EFst _)     = crash
liftDecl (ESnd _)     = crash
liftDecl (EUnit)      = crash

liftDeclUnary :: Partial => forall a b. UnaryExpr a -> Cont (Expr b) (UnaryExpr a)
liftDeclUnary = overUnaryA $ fromGenericA liftDecl

liftDeclBinary :: Partial => forall a b. BinaryExpr a -> Cont (Expr b) (BinaryExpr a)
liftDeclBinary = overBinaryA $ fromGenericA liftDecl

liftDeclCall :: Partial => forall a b. CallExpr a -> Cont (Expr b) (CallExpr a)
liftDeclCall = overCallA $ fromGenericA liftDecl