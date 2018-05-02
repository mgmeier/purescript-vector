-----------------------------------------------------------------------------
--
-- Module      :  Vector3
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Vector3 where

import Prelude
import Data.Vector (Vec(Vec))
import Data.TypeNat (Three)
import Data.Array (insertAt, length, unsafeIndex)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)


type Vec3 = Vec Three

vec3 :: forall a. a -> a -> a -> Vec3 a
vec3 x y z = Vec [x,y,z]

vec3' :: forall a. Array a -> Vec3 a
vec3' array | length array == 3 = Vec array
            | otherwise         = unsafeCrashWith "Vector3>>vec3': wrong array length!"

i3 :: Vec3 Number
i3 = Vec [1.0,0.0,0.0]
j3 :: Vec3 Number
j3 = Vec [0.0,1.0,0.0]
k3 :: Vec3 Number
k3 = Vec [0.0,0.0,1.0]

get3X :: forall a. Vec3 a -> a
get3X (Vec v) = unsafePartial $ unsafeIndex v 0

get3Y :: forall a. Vec3 a -> a
get3Y (Vec v) = unsafePartial $ unsafeIndex v 1

get3Z :: forall a. Vec3 a -> a
get3Z (Vec v) = unsafePartial $ unsafeIndex v 2

set3X :: forall a. a -> Vec3 a -> Vec3 a
set3X n (Vec v) = Vec (unsafePartial $ fromJust (insertAt 0 n v))

set3Y :: forall a. a -> Vec3 a -> Vec3 a
set3Y n (Vec v) = Vec (unsafePartial $ fromJust (insertAt 1 n v))

set3Z :: forall a. a -> Vec3 a -> Vec3 a
set3Z n (Vec v) = Vec (unsafePartial $ fromJust (insertAt 2 n v))

-- | The cross product of a and b
cross :: forall a. (EuclideanRing a) => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec [x1,y1,z1]) (Vec [x2,y2,z2]) = Vec [y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2 - y1*x2]
cross _ _ = unsafeCrashWith "Vector3>>cross: impossible!"
