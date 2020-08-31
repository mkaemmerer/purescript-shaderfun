module Data.VectorSpace
  ( class VectorSpace
  , class InnerSpace
  , class AdditiveGroup
  , addV
  , subV
  , negateV
  , zeroV
  , dot
  , scale
  , scaleLeft
  , scaleRight
  , divV
  , lerp
  , sumV
  , averageV
  , magnitudeSquared
  , magnitude
  , (^+^)
  , (^-^)
  , (<.>)
  , (*^)
  , (^*)
  , (^/)
  ) where

import Prelude

import Data.Foldable (class Foldable, foldl, sum)
import Data.Tuple (Tuple(..), fst, snd)
import Math (sqrt)

-- | The `AdditiveGroup` type class is isomorphic to Group,
-- | but useful when we want to specify the (+, 0, negate) group,
-- | and not the (*, 1, recip) group.
-- |
-- | Instances should satisfy these laws:
-- |
-- | - `zeroV ^+^ a == a`
-- | - `a ^+^ zeroV == a`
-- | - `a ^-^ a == zeroV`
-- | - `negateV a == zeroV ^-^ a`
-- | - `a ^+^ b == b ^+^ a`
-- | - `a ^+^ (b ^+^ c) == (a ^+^ b) ^+^ c`
class AdditiveGroup v where
  zeroV :: v
  addV :: v -> v -> v
  subV :: v -> v -> v
  negateV :: v -> v

-- | The `VectorSpace` type class is for vectors and their associated scalar types
-- |
-- | Instances should satisfy these laws:
-- |
-- | - s *^ (u ^+^ v) == (s *^ u) ^+^ (s *^ v)`
-- | - (s ^+^ t) *^ u == (s *^ u) ^+^ (t *^ u)`
class (AdditiveGroup vector, AdditiveGroup scalar) <= VectorSpace scalar vector | vector -> scalar where
  scale :: scalar -> vector -> vector
-- TODO: any other laws needed for this to be well-behaved?

-- | The `InnerSpace` type class is for vectors that have a dot product
-- |
-- | Instances should satisfy these laws:
-- |
-- | - u <.> v == v <.> u
class (VectorSpace scalar vector) <= InnerSpace scalar vector | vector -> scalar where
  dot :: vector -> vector -> scalar
-- TODO: any other laws needed for this to be well-behaved?

scaleLeft :: forall v s. VectorSpace s v => s -> v -> v
scaleLeft = scale

scaleRight :: forall v s. VectorSpace s v => v -> s -> v
scaleRight = flip scale

divV :: forall v s. VectorSpace s v => DivisionRing s => v -> s -> v
divV v s = v ^* (recip s)

-- | Linear interpolation between two vectors
lerp :: forall v s. VectorSpace s v => DivisionRing s => v -> v -> s -> v
lerp a b t = a ^+^ t *^ (b ^-^ a)

sumV :: forall v s f. Foldable f => VectorSpace s v => f v -> v
sumV = foldl addV zeroV

averageV ::
  forall v s f.
  Functor f =>
  Foldable f => DivisionRing s => VectorSpace s v => f v -> v
averageV cs = sumV cs ^/ count
  where
  count = sum $ one <$ cs

infixl 6 addV as ^+^
infixl 6 subV as ^-^
infixr 7 dot as <.>
infixr 7 scale as *^
infixr 7 scaleRight as ^*
infixr 7 divV as ^/

magnitudeSquared :: forall v s. InnerSpace s v => v -> s
magnitudeSquared v = v <.> v

magnitude :: forall v. InnerSpace Number v => v -> Number
magnitude = magnitudeSquared >>> sqrt 

-- Number instance
instance additiveGroupNumber :: AdditiveGroup Number where
  zeroV = zero
  addV = add
  subV = sub
  negateV = negate

instance vectorSpaceNumber :: VectorSpace Number Number where
  scale = mul

instance innerSpaceNumber :: InnerSpace Number Number where
  dot = mul

-- Tuple instance
instance additiveGroupTuple :: (AdditiveGroup a, AdditiveGroup b) => AdditiveGroup (Tuple a b) where
  zeroV = Tuple zeroV zeroV
  addV e1 e2 = Tuple (fst e1 ^+^ fst e2) (snd e1 ^+^ snd e2)
  subV e1 e2 = Tuple (fst e1 ^-^ fst e2) (snd e1 ^-^ snd e2)
  negateV e = Tuple (negateV $ fst e) (negateV $ snd e)

instance vectorSpaceTuple :: (VectorSpace s a, VectorSpace s b) => VectorSpace s (Tuple a b) where
  scale s e = Tuple (s *^ fst e) (s *^ snd e)

instance innerSpaceTuple :: (InnerSpace s a, InnerSpace s b) => InnerSpace s (Tuple a b) where
  dot u v = (fst u <.> fst v) ^+^ (snd u <.> snd v)
