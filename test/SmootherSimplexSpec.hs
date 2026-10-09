module SmootherSimplexSpec (test_golden_smoothersimplex) where

import Golden.Util
import Numeric.Noise
import Test.Tasty (TestTree, testGroup)

test_golden_smoothersimplex :: TestTree
test_golden_smoothersimplex =
  testGroup
    "SmootherSimplex Golden Tests"
    [ testGroup "2D Grid Tests" smootherSimplex2DGridTests
    , testGroup "2D Sparse Tests" smootherSimplex2DSparseTests
    , testGroup "3D Grid Tests" smootherSimplex3DGridTests
    , testGroup "3D Sparse Tests" smootherSimplex3DSparseTests
    ]

smootherSimplex2DGridTests :: [TestTree]
smootherSimplex2DGridTests = golden2DImageTests "smoothersimplex" defaultSeeds smootherSimplex2

smootherSimplex2DSparseTests :: [TestTree]
smootherSimplex2DSparseTests = golden2DSparseTests "smoothersimplex" defaultSeeds smootherSimplex2

smootherSimplex3DGridTests :: [TestTree]
smootherSimplex3DGridTests = golden3DImageTests "smoothersimplex" defaultSeeds smootherSimplex3

smootherSimplex3DSparseTests :: [TestTree]
smootherSimplex3DSparseTests = golden3DSparseTests "smoothersimplex" defaultSeeds smootherSimplex3
