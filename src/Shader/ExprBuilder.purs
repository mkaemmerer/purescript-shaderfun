module Shader.ExprBuilder
  ( Builder
  , BuilderState
  , ExprBuilder
  , ShaderFunc
  , decl
  , match
  , rec
  , runExprBuilder
  , type (|>)
  ) where

import Prelude hiding (unit)

import Control.Monad.State (State, get, modify, runState)
import Data.Either (Either)
import Data.Tuple (Tuple(..))
import Shader.Expr (class TypedExpr, Expr(..), bindE, matchE, recE)
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

newVar :: Builder String
newVar = do
  { count } <- get
  let name = "v_" <> show count
  _ <- modify $ _ { count = count+1 }
  pure name

-- | `decl` yields an expression which is semantically equivalent
-- | but could be more operationally efficient.
-- | Use `decl` to avoid reevaluating common sub expressions.
decl :: forall t. (TypedExpr t) => Expr t -> ExprBuilder t
decl e = do
  name <- newVar
  let build b = bindE name e b
  { cont } <- get
  _ <- modify $ _ { cont = build >>> (eraseType cont) }
  pure $ EVar name

-- | `match` pattern matches on an expression of type `Either a b`
-- | and chooses the left or right branch accordingly
match :: forall a b c. Expr (Either a b) -> (Expr a -> Expr c) -> (Expr b -> Expr c) -> ExprBuilder c
match e l r = do
  name <- newVar
  pure $ matchE e name l r

-- | `rec` performs bounded recursion up to a given depth.
-- | Recursive function should yield an expression with type `Either t t`
-- | where right values denotes a final result, and left values denote in progress computation.
-- | Use `done` and `loop` to tag expressions.
rec :: forall t. (TypedExpr t) => Int -> (Expr t -> ExprBuilder (Either t t)) -> Expr t -> ExprBuilder t
rec n f seed = do
  name <- newVar
  loop <- buildSubroutine $ f $ EVar name
  { cont } <- get
  let build b = recE n name seed loop b
  _ <- modify $ _ { cont = build >>> (eraseType cont) }
  pure $ EVar name

buildSubroutine :: forall t. ExprBuilder t -> ExprBuilder t
buildSubroutine b = do
  { count } <- get
  let Tuple e { count: count', cont } = runState b { count, cont: identity }
  let e' = cont e
  _ <- modify $ _ { count = count' }
  pure e'

-- | Run an expression builder and yield the result
runExprBuilder :: forall t. ExprBuilder t -> Expr t
runExprBuilder builder = cont e
  where
    Tuple e state = runState builder emptyState
    { cont } = state
