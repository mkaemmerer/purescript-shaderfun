module Data.Complex (Complex(..)) where

import Prelude
import Data.VectorSpace (class AdditiveGroup, class InnerSpace, class VectorSpace)

data Complex
  = Complex Number Number

instance eqComplex :: Eq Complex where
  eq (Complex r i) (Complex r' i') = r == r' && i == i'

-- Field instances
instance semiringComplex :: Semiring Complex where
  one = Complex one zero
  zero = Complex zero zero
  add (Complex r i) (Complex r' i') = Complex (r + r') (i + i')
  mul (Complex r i) (Complex r' i') = Complex (r*r' - i*i') (r*i' + r'*i)

instance ringComplex :: Ring Complex where
  sub (Complex r i) (Complex r' i') = Complex (r - r') (i - i')

instance commutativeRingComplex :: CommutativeRing Complex

instance euclideanRingComplex :: EuclideanRing Complex where
  degree _ = 1
  mod _ _ = zero
  div (Complex r i) (Complex r' i') = Complex nr ni
    where
    nr = (r*r' + i*i') / d
    ni = (i*r' - r*i') / d
    d  = r'*r' + i'*i'

instance divisionRingComplex :: DivisionRing Complex where
  recip c = one / c

-- Vector space instances
instance additiveGroupComplex :: AdditiveGroup Complex where
  zeroV = zero
  addV = add
  subV = sub
  negateV (Complex r i) = Complex (-r) (-i)

instance vectorSpaceComplex :: VectorSpace Number Complex where
  scale s (Complex x y) = Complex (s*x) (s*y)

instance innerSpaceComplex :: InnerSpace Number Complex where
  dot (Complex r1 i1) (Complex r2 i2) = r1 * r2 + i1 * i2
