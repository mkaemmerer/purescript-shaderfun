module Shader.ExprBuilder
  ( Builder
  , BuilderState
  , ExprBuilder
  , ShaderFunc
  , class Declarable
  , decl
  , runExprBuilder
  , type (|>)
  ) where

import Prelude hiding (unit)

import Control.Monad.State (State, get, modify, runState)
import Data.Either (Either)
import Data.HeytingAlgebra (ff, tt)
import Data.Tuple (Tuple(..))
import Shader.Expr (class TypedExpr, Expr(..), bindE, fst, ifE, inl, inr, matchE, snd, tuple, unit)
import Unsafe.Coerce (unsafeCoerce)

-- | `Builder` is a monad for building expressions with unique variable names
type Builder a = State BuilderState a
type BuilderState = { count :: Int, cont :: forall a. Expr a -> Expr a }
-- TODO: Can this be decomposed with Cont/ContT/State/StateT

type ExprBuilder a = Builder (Expr a)
type ShaderFunc a b = Expr a -> ExprBuilder b

infixr 4 type ShaderFunc as |>

emptyState :: BuilderState
emptyState = { count: 0, cont: identity }

eraseType :: forall a b. (a -> a) -> (b -> b)
eraseType = unsafeCoerce

-- | The `Declarable` type class is for types that have 
-- | a corresponding representation in GLSL.
-- | 
-- | `decl` yields an expression which is semantically equivalent
-- | but could be more operationally efficient.
-- | Use `decl` to avoid reevaluating common sub expressions.
class Declarable t where
  decl :: Expr t -> ExprBuilder t

instance declarableTyped :: (TypedExpr t) => Declarable t where
  decl = declSingle

else instance declarableUnit :: Declarable Unit where
  decl = declUnit

else instance declarableTuple :: (Declarable a, Declarable b) => Declarable (Tuple a b) where
  decl = declTuple

else instance declarableEither :: (Declarable a, Declarable b) => Declarable (Either a b) where
  decl = declEither

declSingle :: forall t. (TypedExpr t) => Expr t -> ExprBuilder t
declSingle e = do
  { count, cont } <- get
  let name = "v_" <> show count
  let build b = bindE name e b
  _ <- modify $ _ { count = count+1, cont = build >>> (eraseType cont) }
  pure $ EVar name

declUnit :: Expr Unit -> ExprBuilder Unit
declUnit e = pure unit

declTuple :: forall a b. Declarable a => Declarable b => Expr (Tuple a b) -> ExprBuilder (Tuple a b)
declTuple e = do
  let e1 = fst e
  let e2 = snd e
  v1 <- decl e1
  v2 <- decl e2
  pure $ tuple v1 v2

declEither :: forall a b. Declarable a => Declarable b => Expr (Either a b) -> ExprBuilder (Either a b)
declEither e = do
  tag <- decl $ matchE e (const tt) (const ff)
  vl <- decl $ matchE e identity (const default)
  vr <- decl $ matchE e (const default) identity
  let e' = ifE tag (inl vl) (inr vr)
  pure $ e'

default :: forall a. Expr a
default = unsafeCoerce unit

-- | Run an expression builder and yield the result
runExprBuilder :: forall t. ExprBuilder t -> Expr t
runExprBuilder builder = cont e
  where
    Tuple e state = runState builder emptyState
    { cont } = state
