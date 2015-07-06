module Test.Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Test.Assert (assert)
import Data.Array.Unsafe (unsafeIndex)
import qualified Data.TypeNat as TN
import qualified Data.Vector as V
import qualified Data.Vector2 as V
import qualified Data.Vector3 as V
import qualified Data.Vector4 as V

main = testVector

a2 = [1.0,2.0]
a3 = [1.0,2.0,3.0]
a4 = [1.0,2.0,3.0,4.0]

testVector = do

  log "toArray after fromArray results in equality"
  assert $ (V.toArray <<< V.fromArray ::  Array Number -> V.Vec2 Number) a2 == a2
  assert $ (V.toArray <<< (V.fromArray :: Array Number -> V.Vec3 Number)) a3 == a3
  assert $ (V.toArray <<< (V.fromArray :: Array Number -> V.Vec4 Number)) a4 == a4

  log "fromArray after toArray results in equality"
  assert $ (V.fromArray <<< V.toArray) (V.vec2' a2) == V.vec2' a2
  assert $ (V.fromArray <<< V.toArray) (V.vec3' a3) == V.vec3' a3
  assert $ (V.fromArray <<< V.toArray) (V.vec4' a4) == V.vec4' a4

  log "add "
  assert $ (V.fromArray a2 + V.fromArray a2) == V.vec2 (unsafeIndex a2 0 + unsafeIndex a2 0)
                                                        (unsafeIndex a2 1 + unsafeIndex a2 1)
  assert $ (V.fromArray a3 + V.fromArray a3) == V.vec3 (unsafeIndex a3 0 + unsafeIndex a3 0)
                                                        (unsafeIndex a3 1 + unsafeIndex a3 1)
                                                        (unsafeIndex a3 2 + unsafeIndex a3 2)
  assert $ (V.fromArray a4 + V.fromArray a4) == V.vec4 (unsafeIndex a4 0 + unsafeIndex a4 0)
                                                        (unsafeIndex a4 1 + unsafeIndex a4 1)
                                                        (unsafeIndex a4 2 + unsafeIndex a4 2)
                                                        (unsafeIndex a4 3 + unsafeIndex a4 3)

  log "Multiply "
  assert $ (V.fromArray a2 * V.fromArray a2) == V.vec2 (unsafeIndex a2 0 * unsafeIndex a2 0)
                                                        (unsafeIndex a2 1 * unsafeIndex a2 1)
  assert $ (V.fromArray a3 * V.fromArray a3) == V.vec3 (unsafeIndex a3 0 * unsafeIndex a3 0)
                                                        (unsafeIndex a3 1 * unsafeIndex a3 1)
                                                        (unsafeIndex a3 2 * unsafeIndex a3 2)
  assert $ (V.fromArray a4 * V.fromArray a4) == V.vec4 (unsafeIndex a4 0 * unsafeIndex a4 0)
                                                        (unsafeIndex a4 1 * unsafeIndex a4 1)
                                                        (unsafeIndex a4 2 * unsafeIndex a4 2)
                                                        (unsafeIndex a4 3 * unsafeIndex a4 3)

  log "Zero "
  assert $ V.fromArray a2 + zero :: V.Vec2 Number == V.fromArray a2
  assert $ V.fromArray a3 + zero :: V.Vec3 Number == V.fromArray a3
  assert $ V.fromArray a4 + zero :: V.Vec4 Number == V.fromArray a4

  log "One "
  assert $ V.fromArray a2 * one :: V.Vec2 Number == V.fromArray a2
  assert $ V.fromArray a3 * one :: V.Vec3 Number == V.fromArray a3
  assert $ V.fromArray a4 * one :: V.Vec4 Number == V.fromArray a4
