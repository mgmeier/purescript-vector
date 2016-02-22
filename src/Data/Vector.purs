-----------------------------------------------------------------------------
--
-- Module      :  Matrix
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Vector where

import Prelude (class Num, class Semiring, class Apply, class Functor, class Show, class Eq, (+), (*), (<$>), (/), (<<<), negate, (-), (<>), map, show, (++), eq, otherwise, (==))
import Data.Array (zipWith, length, replicate)
import Data.Monoid (mempty)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.TypeNat (class Sized, sized)
import Control.Apply (lift2)
import Math (sqrt)
import Type.Proxy (Proxy(..))
import Extensions (fail)

newtype Vec s a = Vec (Array a)

fill :: forall s a. (Num a, Sized s) => a -> Vec s a
fill a = Vec (replicate (sized (Proxy :: Proxy s)) a)

fromArray :: forall s a. (Sized s) => Array a -> Vec s a
fromArray l =
  let res = Vec l
  in case sized (Proxy :: Proxy s) of
        i | i == length l -> res
          | otherwise     -> fail "Vector>>fromArray: wrong array length!"

toArray :: forall s a. Vec s a -> Array a
toArray (Vec a) = a

instance eqVec :: (Eq a) => Eq (Vec s a) where
  eq (Vec l) (Vec r) = l `eq` r

instance showVec :: (Show a) => Show (Vec s a) where
  show (Vec l) = "Vec " ++ show l

instance functorVec :: Functor (Vec s) where
  map f (Vec l) = Vec (f `map` l)

instance applyVec :: Apply (Vec s) where
  apply (Vec f) (Vec a) = Vec (zipWith (\f' a' -> f' a') f a)

instance foldableVector :: Foldable (Vec s) where
  foldr f z (Vec xs) = foldr f z xs
  foldl f z (Vec xs) = foldl f z xs
  foldMap f xs = foldr (\x acc -> f x <> acc) mempty xs

vAdd :: forall a s. (Num a) => Vec s a -> Vec s a -> Vec s a
vAdd = lift2 (+)

vSub :: forall a s. (Num a) => Vec s a -> Vec s a -> Vec s a
vSub = lift2 (-)

vMul :: forall a s. (Num a) => Vec s a -> Vec s a -> Vec s a
vMul = lift2 (*)

vNegate :: forall a s. (Num a) => Vec s a -> Vec s a
vNegate v = negate <$> v

instance semiringVector :: (Sized s) => Semiring (Vec s Number) where
  add = vAdd
  zero = fill 0.0
  mul = vMul
  one = fill 1.0

-- | The normalized direction from a to b: (a - b) / |a - b|
direction :: forall s. Vec s Number -> Vec s Number -> Vec s Number
direction v1 v2 = normalize (vSub v1 v2)

-- | The length of the given vector: |a|
vlengthSquared :: forall s. Vec s Number -> Number
vlengthSquared v = foldl (+) 0.0 ((\e -> e * e) <$> v)

-- | The length of the given vector: |a|
vlength :: forall s. Vec s Number -> Number
vlength = sqrt <<< vlengthSquared
  -- :: forall a b. f (a -> b) -> f a -> f b

-- |A unit vector with the same direction as the given vector: a / |a|
normalize :: forall s. Vec s Number -> Vec s Number
normalize v =
  let im = 1.0 / vlength v
  in (\e -> e * im) <$> v

-- | The distance between two vectors.
distanceSquared :: forall s. Vec s Number -> Vec s Number -> Number
distanceSquared v1 v2 = foldl (+) 0.0 ((\e -> e * e) <$> (vSub v1 v2))

-- | The distance between two vectors.
distance :: forall s. Vec s Number -> Vec s Number -> Number
distance v1 v2 = sqrt (distanceSquared v1 v2)

-- | Multiply the vector by a scalar: s * v
scale :: forall a s. (Num a) => a -> Vec s a -> Vec s a
scale s v = (\e -> e * s) <$> v

-- | The dot product of a and b
dot :: forall s . Vec s Number -> Vec s Number -> Number
dot v1 v2 = foldl (+) 0.0 (vMul v1 v2)
