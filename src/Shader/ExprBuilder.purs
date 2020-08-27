module Shader.ExprBuilder (Builder, BuilderState, ExprBuilder, ShaderFunc, class Declarable, decl, runExprBuilder) where

import Prelude

import Control.Monad.State (State, get, modify, runState)
import Data.Tuple (Tuple(..))
import Shader.Expr (class TypedExpr, Expr(..), bindE, fst, snd, tuple)
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


class Declarable t where
  decl :: Expr t -> ExprBuilder t

instance declarableTyped :: (TypedExpr t) => Declarable t where
  decl = declSingle

else instance declarableTuple :: (Declarable a, Declarable b) => Declarable (Tuple a b) where
  decl = declTuple

declSingle :: forall t. (TypedExpr t) => Expr t -> ExprBuilder t
declSingle e = do
  { count, cont } <- get
  let name = "v_" <> show count
  let build b = bindE name e b
  _ <- modify $ _ { count = count+1, cont = build >>> (eraseType cont) }
  pure $ EVar name

declTuple :: forall a b. Declarable a => Declarable b => Expr (Tuple a b) -> ExprBuilder (Tuple a b)
declTuple e = do
  let e1 = fst e
  let e2 = snd e
  v1 <- decl e1
  v2 <- decl e2
  pure $ tuple v1 v2


runExprBuilder :: forall t. ExprBuilder t -> Expr t
runExprBuilder builder = cont e
  where
    Tuple e state = runState builder emptyState
    { cont } = state
