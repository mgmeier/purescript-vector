-----------------------------------------------------------------------------
--
-- Module      :  Vector4
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Vector4 where

import Prelude (otherwise, (==))
import Data.Vector (Vec(Vec))
import Data.TypeNat (Four)
import Data.Array (insertAt, length)
import Data.Array.Unsafe (unsafeIndex)
import Data.Maybe.Unsafe (fromJust)
import Extensions (fail)

type Vec4 = Vec Four

vec4 :: forall a. a -> a -> a -> a -> Vec4 a
vec4 x y z u = Vec [x,y,z,u]

vec4' :: forall a. Array a -> Vec4 a
vec4' array | length array == 4 = Vec array
            | otherwise         = fail "Vector4>>vec4': wrong array length!"

i4 :: Vec4 Number
i4 = Vec [1.0,0.0,0.0,0.0]
j4 :: Vec4 Number
j4 = Vec [0.0,1.0,0.0,0.0]
k4 :: Vec4 Number
k4 = Vec [0.0,0.0,1.0,0.0]
l4 :: Vec4 Number
l4 = Vec [0.0,0.0,0.0,1.0]

get4X :: forall a. Vec4 a -> a
get4X (Vec v) = unsafeIndex v 0

get4Y :: forall a. Vec4 a -> a
get4Y (Vec v) = unsafeIndex v 1

get4Z :: forall a. Vec4 a -> a
get4Z (Vec v) = unsafeIndex v 2

get4U :: forall a. Vec4 a -> a
get4U (Vec v) = unsafeIndex v 3

set4X :: forall a. a -> Vec4 a -> Vec4 a
set4X n (Vec v) = Vec (fromJust (insertAt 0 n v))

set4Y :: forall a. a -> Vec4 a -> Vec4 a
set4Y n (Vec v) = Vec (fromJust (insertAt 1 n v))

set4Z :: forall a. a -> Vec4 a -> Vec4 a
set4Z n (Vec v) = Vec (fromJust (insertAt 2 n v))

set4U :: forall a. a -> Vec4 a -> Vec4 a
set4U n (Vec v) = Vec (fromJust (insertAt 3 n v))
