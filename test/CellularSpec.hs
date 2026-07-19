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
    , testGroup "3D Grid Tests" cellular3DGridTests
    , testGroup "3D Sparse Tests" cellular3DSparseTests
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

-- Full 3D image goldens would add ~200 PNGs; sparse JSON covers the whole
-- config matrix, images sample one distance function across result shapes.
cellular3DImageConfigs :: [(CellularDistanceFn, CellularResult)]
cellular3DImageConfigs =
  [ (DistEuclidean, CellValue)
  , (DistEuclidean, Distance)
  , (DistEuclidean, Distance2Add)
  ]

cellular3DGridTests :: [TestTree]
cellular3DGridTests =
  [ goldenImageTest3D "cellular" variant (cellular3 config) seed zOffset
  | (distFn, result) <- cellular3DImageConfigs
  , let config = defaultCellularConfig{cellularDistanceFn = distFn, cellularResult = result}
  , seed <- cellularSeeds
  , (idx, zOffset) <- zip [0 :: Int ..] sliceOffsets3D
  , let variant = "3d-" <> show distFn <> "-" <> show result <> "-seed_" <> show seed <> "-slice_" <> show idx
  ]

cellular3DSparseTests :: [TestTree]
cellular3DSparseTests =
  [ goldenSparseTest3D "cellular" variant (cellular3 config) seed
  | (distFn, result) <- allCellularConfigs
  , let config = defaultCellularConfig{cellularDistanceFn = distFn, cellularResult = result}
  , seed <- cellularSeeds
  , let variant = "3d-" <> show distFn <> "-" <> show result <> "-seed" <> show seed
  ]
