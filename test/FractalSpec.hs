{-# LANGUAGE OverloadedStrings #-}

module FractalSpec (test_golden_fractal) where

import Golden.Util
import Numeric.Noise
import Test.Tasty (TestTree, testGroup)

test_golden_fractal :: TestTree
test_golden_fractal =
  testGroup
    "Fractal Golden Tests"
    [ testGroup "2D Grid Tests" fractal2DGridTests
    , testGroup "2D Sparse Tests" fractal2DSparseTests
    , testGroup "3D Grid Tests" fractal3DGridTests
    , testGroup "3D Sparse Tests" fractal3DSparseTests
    , testGroup "2D Weighted Sparse Tests" fractal2DWeightedSparseTests
    , testGroup "3D Weighted Sparse Tests" fractal3DWeightedSparseTests
    ]

-- Fractal types to test
data FractalType = FBM | Billow | Ridged | PingPong
  deriving (Show, Eq, Enum, Bounded)

-- Apply the fractal type to a 2D noise function
applyFractal2D :: FractalType -> Noise2 Double -> Noise2 Double
applyFractal2D FBM = fractal2 defaultFractalConfig
applyFractal2D Billow = billow2 defaultFractalConfig
applyFractal2D Ridged = ridged2 defaultFractalConfig
applyFractal2D PingPong = pingPong2 defaultFractalConfig defaultPingPongStrength

-- Apply the fractal type to a 3D noise function
applyFractal3D :: FractalType -> Noise3 Double -> Noise3 Double
applyFractal3D FBM = fractal3 defaultFractalConfig
applyFractal3D Billow = billow3 defaultFractalConfig
applyFractal3D Ridged = ridged3 defaultFractalConfig
applyFractal3D PingPong = pingPong3 defaultFractalConfig defaultPingPongStrength

fractal2DGridTests :: [TestTree]
fractal2DGridTests =
  [ goldenImageTest2D "fractal" variant (applyFractal2D fractalType perlin2) seed
  | fractalType <- [minBound .. maxBound]
  , seed <- cellularSeeds
  , let variant = show fractalType ++ "-perlin-2d-seed" ++ show seed
  ]

fractal2DSparseTests :: [TestTree]
fractal2DSparseTests =
  [ goldenSparseTest2D "fractal" variant (applyFractal2D fractalType perlin2) seed
  | fractalType <- [minBound .. maxBound]
  , seed <- cellularSeeds
  , let variant = show fractalType ++ "-perlin-2d-seed" ++ show seed
  ]

fractal3DGridTests :: [TestTree]
fractal3DGridTests =
  [ goldenImageTest3D "fractal" variant (applyFractal3D fractalType perlin3) seed zOffset
  | fractalType <- [minBound .. maxBound]
  , seed <- cellularSeeds
  , (idx, zOffset) <- zip [0 :: Int ..] sliceOffsets3D
  , let variant = show fractalType ++ "-perlin-3d-seed_" ++ show seed ++ "-slice_" ++ show idx
  ]

fractal3DSparseTests :: [TestTree]
fractal3DSparseTests =
  [ goldenSparseTest3D "fractal" variant (applyFractal3D fractalType perlin3) seed
  | fractalType <- [minBound .. maxBound]
  , seed <- cellularSeeds
  , let variant = show fractalType ++ "-perlin-3d-seed" ++ show seed
  ]

weightedFractalConfig :: FractalConfig Double
weightedFractalConfig = defaultFractalConfig{weightedStrength = 0.5}

applyWeighted2D :: FractalType -> Noise2 Double -> Noise2 Double
applyWeighted2D FBM = fractal2 weightedFractalConfig
applyWeighted2D Billow = billow2 weightedFractalConfig
applyWeighted2D Ridged = ridged2 weightedFractalConfig
applyWeighted2D PingPong = pingPong2 weightedFractalConfig defaultPingPongStrength

applyWeighted3D :: FractalType -> Noise3 Double -> Noise3 Double
applyWeighted3D FBM = fractal3 weightedFractalConfig
applyWeighted3D Billow = billow3 weightedFractalConfig
applyWeighted3D Ridged = ridged3 weightedFractalConfig
applyWeighted3D PingPong = pingPong3 weightedFractalConfig defaultPingPongStrength

fractal2DWeightedSparseTests :: [TestTree]
fractal2DWeightedSparseTests =
  [ goldenSparseTest2D "fractal" variant (applyWeighted2D fractalType perlin2) seed
  | fractalType <- [minBound .. maxBound]
  , seed <- cellularSeeds
  , let variant = show fractalType ++ "-perlin-2d-weighted-seed" ++ show seed
  ]
    ++ [ goldenSparseTest2D
           "fractal"
           "FBM-perlin-scaled-2d-weighted-seed42"
           (fractal2 weightedFractalConfig (perlin2 * 2))
           42
       ]

fractal3DWeightedSparseTests :: [TestTree]
fractal3DWeightedSparseTests =
  [ goldenSparseTest3D "fractal" variant (applyWeighted3D fractalType perlin3) seed
  | fractalType <- [minBound .. maxBound]
  , seed <- cellularSeeds
  , let variant = show fractalType ++ "-perlin-3d-weighted-seed" ++ show seed
  ]
    ++ [ goldenSparseTest3D
           "fractal"
           "FBM-perlin-scaled-3d-weighted-seed42"
           (fractal3 weightedFractalConfig (perlin3 * 2))
           42
       ]
