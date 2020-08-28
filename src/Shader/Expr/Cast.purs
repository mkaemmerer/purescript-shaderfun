module Shader.Expr.Cast (class Castable, cast, class Destructurable, from) where

import Data.Color (Color)
import Data.Complex (Complex)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Vec2 (Vec2)
import Shader.Expr (Expr, bool, fromColor, fromComplex, fromVec2, fst, inl, inr, num, projB, projG, projImaginary, projR, projReal, projX, projY, snd, tuple)


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


class Destructurable t record | t -> record where
  from :: Expr t -> record

instance destructurableVec2 :: Destructurable Vec2 { x :: Expr Number, y :: Expr Number } where
  from v = { x: projX v, y: projY v }

instance destructurableComplex :: Destructurable Complex { r :: Expr Number, i :: Expr Number } where
  from c = { r: projReal c, i: projImaginary c }

instance destructurableColor :: Destructurable Color { r :: Expr Number, g :: Expr Number, b :: Expr Number } where
  from c = { r: projR c, g: projG c, b: projB c }

instance destructurableTuple :: Destructurable (Tuple a b) (Tuple (Expr a) (Expr b)) where
  from t = Tuple (fst t) (snd t)
