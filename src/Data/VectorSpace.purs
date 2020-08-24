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
  , magnitudeSquared
  , (^+^)
  , (^-^)
  , (<.>)
  , (*^)
  , (^*)
  , (^/)
  ) where

import Prelude

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

infixl 6 addV as ^+^
infixl 6 subV as ^-^
infixr 7 dot as <.>
infixr 7 scale as *^
infixr 7 scaleRight as ^*
infixr 7 divV as ^/

magnitudeSquared :: forall v s. InnerSpace s v => v -> s
magnitudeSquared v = v <.> v