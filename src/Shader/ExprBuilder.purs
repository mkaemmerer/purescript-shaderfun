module Shader.ExprBuilder (Builder, ExprBuilder, ShaderFunc, BuilderState, decl, runExprBuilder) where

import Prelude

import Control.Monad.State (State, gets, modify, runState)
import Data.Tuple (Tuple(..))
import Shader.Expr (class TypedExpr, Expr(..), bindE)
import Unsafe.Coerce (unsafeCoerce)

-- TODO: Can this be decomposed with Cont/ContT/State/StateT
-- TODO: Hide builder state with existential type?
type BuilderState = { count :: Int, cont :: forall a. Expr a -> Expr a }
type Builder a = State BuilderState a

type ExprBuilder a = Builder (Expr a)
type ShaderFunc a b = Expr a -> ExprBuilder b

emptyState :: BuilderState
emptyState = { count: 0, cont: identity }

eraseType :: forall a b. (a -> a) -> (b -> b)
eraseType = unsafeCoerce

newVar :: forall t. ExprBuilder t
newVar = do
  count <- gets _.count
  _ <- modify $ _ { count = count+1 }
  pure $ EVar $ "v_" <> show count

varName :: forall t. Expr t -> String
varName (EVar name) = name
varName _ = ""

decl :: forall t. (TypedExpr (Expr t)) => Expr t -> ExprBuilder t
decl e = do
  v <- newVar
  k <- gets _.cont
  let build b = bindE (varName v) e b
  _ <- modify $ _ { cont = build >>> (eraseType k) }
  pure v

runExprBuilder :: forall t. ExprBuilder t -> Expr t
runExprBuilder builder = cont e
  where
    Tuple e state = runState builder emptyState
    { cont } = state