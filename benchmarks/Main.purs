module Benchmark.Main where

import Prelude
import Data.Array((..))
import Control.Monad.Eff(Eff(..))
import Control.Monad.Eff.Random
import Data.Foldable(foldr)
import qualified Data.Vector3 as V
import qualified Data.Vector as V
import Benchotron.Core
import Benchotron.UI.Console

vecSum :: forall e. Benchmark (random :: RANDOM | e)
vecSum = mkBenchmark
  { slug: "vecSum"
  , title: "Finding the sum of two vectors"
  , sizes: (1..5) <#> (*1000)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: randomArray randomVec3
  , functions: [ benchFn "vecSum" (foldr (+) (V.vec3'[0.0,0.0,0.0]))
               ]
  }

vecSumPrim :: forall e. Benchmark (random :: RANDOM | e)
vecSumPrim = mkBenchmark
  { slug: "vecSumPrim"
  , title: "Finding the sum of two vectors"
  , sizes: (1..5) <#> (*1000)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: randomArray randomVec3
  , functions: [ benchFn "vecSumPrim" (foldr V.vAdd (V.vec3'[0.0,0.0,0.0]))
               ]
  }

randomVec3 :: forall e. Eff (random :: RANDOM | e) (V.Vec3 Number)
randomVec3 = do
    x <- random
    y <- random
    z <- random
    return (V.vec3' [x,y,z])

randomArray :: forall e a. Eff (random :: RANDOM | e) a -> Int -> Eff (random :: RANDOM | e) (Array a)
randomArray generator size = sequence $ map (\ _ -> generator) (0..(size-1))

main = runSuite [vecSum, vecSumPrim]
