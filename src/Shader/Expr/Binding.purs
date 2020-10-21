module Shader.Expr.Binding (makeBindings, liftBindings) where

import Prelude

import Control.Monad.Cont (Cont, cont, runCont)
import Control.Monad.State (State, evalState, gets, modify)
import Data.HashMap (HashMap, empty, insert, lookup, singleton, unionWith)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, over, over2, unwrap, wrap)
import Shader.Expr (Expr(..), bindE)
import Shader.Expr.Traversal (foldExpr, onA)
import Shader.Expr.Untyped (UntypedExpr, mkUntypedExpr)
import Unsafe.Coerce (unsafeCoerce)


-- Count how many times an expression is used as a subexpression
newtype ExprCounts = ExprCounts (HashMap UntypedExpr Int)
derive instance newtypeExprCounts :: Newtype ExprCounts _

instance semigroupExprCounts :: Semigroup ExprCounts where
  append = over2 ExprCounts $ unionWith (+)

instance monoidExprCounts :: Monoid ExprCounts where
  mempty = wrap empty

single :: forall a. Expr a -> ExprCounts
single e = wrap $ singleton (mkUntypedExpr e) 1

makeCounts :: forall a. Expr a -> ExprCounts
makeCounts e = foldExpr makeCounts e <> single e

lookupCounts :: forall a. Expr a -> ExprCounts -> Int
lookupCounts e counts = fromMaybe 0 count 
  where
    count = lookup (mkUntypedExpr e) (unwrap counts)


-- A map of sub-expressions to their substitutions
newtype ExprSubs = ExprSubs (HashMap UntypedExpr UntypedExpr)
derive instance newtypeExprSubs :: Newtype ExprSubs _

emptySubs :: ExprSubs
emptySubs = wrap empty

insertSub :: forall a. Expr a -> Expr a -> ExprSubs -> ExprSubs
insertSub e1 e2 = over ExprSubs $ insert (mkUntypedExpr e1) (mkUntypedExpr e2)

lookupSub :: forall a. Expr a -> ExprSubs -> Maybe (Expr a)
lookupSub e subs = asTyped <$> lookup (mkUntypedExpr e) (unwrap subs)
  where
    asTyped = unwrap >>> eraseType


-- A monad for introducing bound expressions
type Builder a = State BuilderState a
type BuilderState = { count :: Int, counts :: ExprCounts, subs :: ExprSubs }

newVar :: Builder String
newVar = do
  count <- gets _.count
  _ <- modify $ _ { count = count+1 }
  pure $ "v" <> show count

getSub :: forall a. Expr a -> Builder (Maybe (Expr a))
getSub e = do
  subs <- gets _.subs
  pure $ lookupSub e subs

getCount :: forall a. Expr a -> Builder Int
getCount e = do
  counts <- gets _.counts
  pure $ lookupCounts e counts

makeSub :: forall a. Expr a -> Builder (Expr a)
makeSub e = do
  e' <- onA makeBinds e
  n <- newVar
  subs <- gets _.subs
  _ <- modify $ _ { subs = insertSub e (EVar n) subs }
  pure $ bindE n e' (EVar n)

makeBinds :: forall a. Expr a -> Builder (Expr a)
makeBinds e@(EVar _)  = pure e
makeBinds e@(ENum _)  = pure e
makeBinds e@(EBool _) = pure e
makeBinds e = do
  s <- getSub e
  case s of
    (Just e') -> pure e'
    Nothing   -> do
      b  <- needsBind e
      if b
        then makeSub e
        else onA makeBinds e

needsBind :: forall a. Expr a -> Builder Boolean
needsBind (ERec _ _ _ _) = pure true
needsBind (EUnary _)     = pure false
needsBind e              = (_ > 1) <$> getCount e

-- Introduce bindings by eliminating common sub-expressions
addBindings :: forall a. Expr a -> Expr a
addBindings e = evalState (makeBinds e) { count: 0, counts: makeCounts e, subs: emptySubs }

-- Hoist declarations to top level
liftBindings :: forall a. Expr a -> Expr a
liftBindings e = runCont (liftBinding e2) identity
  where
    e2 = identity e

liftBinding :: forall a b. Expr a -> Cont (Expr b) (Expr a)
liftBinding (EBind name e1 e2) = do
  let mkBind e1' e2' = EBind name (eraseType e1') e2'
  e1' <- liftBinding e1
  cont \f -> mkBind e1' (runCont (liftBinding e2) f)
liftBinding (ERec n name seed loop) = do
  let mkRec seed' loop' = ERec n name (eraseType seed') (eraseType loop')
  seed' <- liftBinding seed
  let loop' = liftBindings $ eraseType loop
  pure $ mkRec seed' loop'
liftBinding e = onA liftBinding e

eraseType :: forall a b. Expr a -> Expr b
eraseType = unsafeCoerce

-- | Optimize an expression by storing common sub-expressions in variables and reusing them
makeBindings :: forall a. Expr a -> Expr a
makeBindings = addBindings >>> liftBindings
