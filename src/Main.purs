module Main where

import Prelude
import Data.Map as M
import Benchotron.Core (Benchmark, BenchEffects, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Control.Monad.Eff (Eff)
import Data.Array (length, (..))
import Data.Int (toNumber, floor)
import Data.List (List)
import Data.Tuple (Tuple(..))
import Math (pow)
import Test.QuickCheck.Gen (Gen, chooseInt, listOf)

--sizes :: Array Int
sizes = (1..10) <#> toNumber >>> pow 1.5 >>> floor  -- \n -> floor (pow 1.5 (toNumber n))

benchToUnfoldableList :: Benchmark
benchToUnfoldableList =
  let
    genArray :: Int -> Gen (Array (Tuple Int Int))
    genArray n = chooseInt 1 1 <#> (\s -> s..n <#> \i -> Tuple i i)
  in
   mkBenchmark
     { slug: "Map.toUnfoldable"
     , title: "Converting a map to an array using `toUnfoldable`"
     , sizes: sizes
     , sizeInterpretation: "Number of elements in the Map"
     , inputsPerSize: 5
     , gen: \n -> M.fromFoldable <$> genArray n
     , functions: [ benchFn "oldToUnfoldable"
                    (length <<< (M.oldToUnfoldable :: M.Map Int Int -> Array (Tuple Int Int)))
                  , benchFn "newToUnfoldable"
                    (length <<< (M.toUnfoldable :: M.Map Int Int -> Array (Tuple Int Int)))
                  ]
     }

main :: forall eff. Eff (BenchEffects eff) Unit
main = runSuite [benchToUnfoldableList]
