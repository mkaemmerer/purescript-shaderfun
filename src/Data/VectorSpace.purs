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
  , (^+^)
  , (^-^)
  , (<.>)
  , (*^)
  , (^*)
  , (^/)
  ) where

import Prelude

import Data.Foldable (class Foldable, foldl, sum)

class AdditiveGroup v where
  zeroV :: v
  addV :: v -> v -> v
  subV :: v -> v -> v
  negateV :: v -> v

class (AdditiveGroup vector) <= VectorSpace scalar vector | vector -> scalar where
  scale :: scalar -> vector -> vector

class (VectorSpace scalar vector) <= InnerSpace scalar vector | vector -> scalar where
  dot :: vector -> vector -> scalar

scaleLeft :: forall v s. VectorSpace s v => s -> v -> v
scaleLeft = scale

scaleRight :: forall v s. VectorSpace s v => v -> s -> v
scaleRight = flip scale

divV :: forall v s. VectorSpace s v => DivisionRing s => v -> s -> v
divV v s = v ^* (recip s)

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
