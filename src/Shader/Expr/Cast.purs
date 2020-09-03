module Shader.Expr.Cast (class Castable, cast, class Destructurable, from, asUnit, asBoolean, asNumber, asVec2, asVec3, asComplex, asColor) where

import Prelude

import Data.Color (Color)
import Data.Complex (Complex)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Vec2 (Vec2)
import Data.Vec3 (Vec3)
import Shader.Expr (Expr, bool, fromColor, fromComplex, fromUnit, fromVec2, fromVec3, fst, inl, inr, num, projB, projG, projImaginary, projR, projReal, projX, projY, projZ, snd, tuple)
import Unsafe.Coerce (unsafeCoerce)


class Castable t where
  cast :: t -> Expr t

instance castableUnit :: Castable Unit where
  cast = fromUnit

instance castableBoolean :: Castable Boolean where
  cast = bool

instance castableNumber :: Castable Number where
  cast = num

instance castableVec2 :: Castable Vec2 where
  cast = fromVec2

instance castableVec3 :: Castable Vec3 where
  cast = fromVec3

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

instance destructurableVec3 :: Destructurable Vec3 { x :: Expr Number, y :: Expr Number, z :: Expr Number } where
  from v = { x: projX v, y: projY v, z: projZ v }

instance destructurableComplex :: Destructurable Complex { r :: Expr Number, i :: Expr Number } where
  from c = { r: projReal c, i: projImaginary c }

instance destructurableColor :: Destructurable Color { r :: Expr Number, g :: Expr Number, b :: Expr Number } where
  from c = { r: projR c, g: projG c, b: projB c }

instance destructurableTuple :: Destructurable (Tuple a b) (Tuple (Expr a) (Expr b)) where
  from t = Tuple (fst t) (snd t)


asUnit :: forall a. Expr a -> Expr Unit
asUnit = unsafeCoerce

asBoolean :: forall a. Expr a -> Expr Boolean
asBoolean = unsafeCoerce

asNumber :: forall a. Expr a -> Expr Number
asNumber = unsafeCoerce

asVec2 :: forall a. Expr a -> Expr Vec2
asVec2 = unsafeCoerce

asVec3 :: forall a. Expr a -> Expr Vec3
asVec3 = unsafeCoerce

asComplex :: forall a. Expr a -> Expr Complex
asComplex = unsafeCoerce

asColor :: forall a. Expr a -> Expr Color
asColor = unsafeCoerce
