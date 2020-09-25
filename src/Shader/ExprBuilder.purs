module Shader.ExprBuilder
  ( Builder
  , BuilderState
  , ExprBuilder
  , ShaderFunc
  , match
  , rec
  , runExprBuilder
  , type (|>)
  ) where

import Prelude hiding (unit)

import Control.Monad.State (State, evalState, get, modify)
import Data.Either (Either)
import Shader.Expr (Expr(..), matchE, recE)

-- | `Builder` is a monad for building expressions with unique variable names
type Builder a = State BuilderState a
type BuilderState = { count :: Int }

type ExprBuilder a = Builder (Expr a)
type ShaderFunc a b = Expr a -> ExprBuilder b

infixr 4 type ShaderFunc as |>

emptyState :: BuilderState
emptyState = { count: 0 }

newVar :: Builder String
newVar = do
  { count } <- get
  _ <- modify $ _ { count = count+1 }
  pure $ "s" <> show count

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
rec :: forall t. Int -> (Expr t -> ExprBuilder (Either t t)) -> Expr t -> ExprBuilder t
rec n f seed = do
  name <- newVar
  loop <- f $ EVar name
  pure $ recE n name seed loop

-- | Run an expression builder and yield the result
runExprBuilder :: forall t. ExprBuilder t -> Expr t
runExprBuilder builder = evalState builder emptyState
