module Data.Hash (Hash, mkHash, runHash) where

import Prelude

import Data.Hashable (class Hashable, hash)
import Data.Newtype (class Newtype, wrap)

-- | A helper monoid for creating hashes with nice distributions
newtype Hash = Hash (Int -> Int)
derive instance newtypeHash :: Newtype Hash _

instance semigroupHash :: Semigroup Hash where
  append (Hash x) (Hash y) = Hash $ x >>> y

-- NOTE: Hash is (Int -> Int) instead of just Int
-- so that we can get a well behaved unit
instance monoidHash :: Monoid Hash where
  mempty = wrap identity

mkHash :: forall a. Hashable a => a -> Hash
mkHash x = Hash (\y -> (hash x) * 31 + y)

runHash :: Hash -> Int
runHash (Hash f) = f 0