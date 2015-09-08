-----------------------------------------------------------------------------
--
-- Module      :  Vector2
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- | Binding to mjs library
--
-----------------------------------------------------------------------------

module Data.Vector2 where

import Prelude
import Data.Vector
import Data.TypeNat
import Data.Array
import Data.Array.Unsafe (unsafeIndex)
import Math
import Data.Maybe.Unsafe (fromJust)
import Extensions (fail)

type Vec2 = Vec Two

vec2 :: forall a. a -> a -> Vec2 a
vec2 x y = Vec [x,y]

vec2' :: forall a. Array a -> Vec2 a
vec2' array | length array == 2 = Vec array
            | otherwise         = fail "Vector2>>vec2': wrong array length!"

i2 :: Vec2 Number
i2 = Vec [1.0,0.0]

j2 :: Vec2 Number
j2 = Vec [0.0,1.0]

get2X :: forall a. Vec2 a -> a
get2X (Vec v) = unsafeIndex v 0

get2Y :: forall a. Vec2 a -> a
get2Y (Vec v) = unsafeIndex v 1

set2X :: forall a. a -> Vec2 a -> Vec2 a
set2X n (Vec v) = Vec (fromJust (insertAt 0 n v))

set2Y :: forall a. a -> Vec2 a -> Vec2 a
set2Y n (Vec v) = Vec (fromJust (insertAt 1 n v))
