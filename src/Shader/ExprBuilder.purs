module Shader.ExprBuilder (Builder, BuilderState, ExprBuilder, ShaderFunc, decl, runExprBuilder) where

import Prelude

import Control.Monad.State (State, get, modify, runState)
import Data.Tuple (Tuple(..))
import Shader.Expr (class TypedExpr, Expr(..), bindE)
import Unsafe.Coerce (unsafeCoerce)

-- TODO: Can this be decomposed with Cont/ContT/State/StateT
type BuilderState = { count :: Int, cont :: forall a. Expr a -> Expr a }
type Builder a = State BuilderState a

type ExprBuilder a = Builder (Expr a)
type ShaderFunc a b = Expr a -> ExprBuilder b

emptyState :: BuilderState
emptyState = { count: 0, cont: identity }

eraseType :: forall a b. (a -> a) -> (b -> b)
eraseType = unsafeCoerce

decl :: forall t. (TypedExpr (Expr t)) => Expr t -> ExprBuilder t
decl e = do
  { count, cont } <- get
  let name = "v_" <> show count
  let build b = bindE name e b
  _ <- modify $ _ { count = count+1, cont = build >>> (eraseType cont) }
  pure $ EVar name

runExprBuilder :: forall t. ExprBuilder t -> Expr t
runExprBuilder builder = cont e
  where
    Tuple e state = runState builder emptyState
    { cont } = state
