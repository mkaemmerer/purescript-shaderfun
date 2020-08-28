module Shader.Expr.Cast (class Castable, cast) where

import Data.Color (Color)
import Data.Complex (Complex)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Vec2 (Vec2)
import Shader.Expr (Expr, bool, fromColor, fromComplex, fromVec2, inl, inr, num, tuple)


class Castable t where
  cast :: t -> Expr t

instance castableBoolean :: Castable Boolean where
  cast = bool

instance castableNumber :: Castable Number where
  cast = num

instance castableVec2 :: Castable Vec2 where
  cast = fromVec2

instance castableComplex :: Castable Complex where
  cast = fromComplex

instance castableColor :: Castable Color where
  cast = fromColor

instance castableTuple :: (Castable a, Castable b) => Castable (Tuple a b) where
  cast (Tuple a b) = tuple (cast a) (cast b)

instance castableEither :: (Castable a, Castable b) => Castable (Either a b) where
  cast (Left a) = inl (cast a)
  cast (Right b) = inr (cast b)
