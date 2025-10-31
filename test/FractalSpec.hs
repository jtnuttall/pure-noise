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
