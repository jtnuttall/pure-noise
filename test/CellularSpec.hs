{-# LANGUAGE OverloadedStrings #-}

module CellularSpec (test_golden_cellular) where

import Golden.Util
import Numeric.Noise
import Test.Tasty (TestTree, testGroup)

test_golden_cellular :: TestTree
test_golden_cellular =
  testGroup
    "Cellular Golden Tests"
    [ testGroup "Grid Tests" cellularGridTests
    , testGroup "Sparse Tests" cellularSparseTests
    ]

-- All combinations of distance functions and result types
allCellularConfigs :: [(CellularDistanceFn, CellularResult)]
allCellularConfigs = [(df, rt) | df <- [minBound .. maxBound], rt <- [minBound .. maxBound]]

cellularGridTests :: [TestTree]
cellularGridTests =
  [ goldenImageTest2D "cellular" variant (cellular2 config) seed
  | (distFn, result) <- allCellularConfigs
  , let config = defaultCellularConfig{cellularDistanceFn = distFn, cellularResult = result}
  , seed <- cellularSeeds
  , let variant = show distFn ++ "-" <> show result <> "-seed" ++ show seed
  ]

cellularSparseTests :: [TestTree]
cellularSparseTests =
  [ goldenSparseTest2D "cellular" variant (cellular2 config) seed
  | (distFn, result) <- allCellularConfigs
  , let config = defaultCellularConfig{cellularDistanceFn = distFn, cellularResult = result}
  , seed <- cellularSeeds
  , let variant = show distFn <> "-" <> show result <> "-seed" ++ show seed
  ]
