module SuperSimplexSpec (test_golden_supersimplex) where

import Golden.Util
import Numeric.Noise
import Test.Tasty (TestTree, testGroup)

test_golden_supersimplex :: TestTree
test_golden_supersimplex =
  testGroup
    "SuperSimplex Golden Tests"
    [ testGroup "2D Grid Tests" superSimplex2DGridTests
    , testGroup "2D Sparse Tests" superSimplex2DSparseTests
    , testGroup "3D Grid Tests" superSimplex3DGridTests
    , testGroup "3D Sparse Tests" superSimplex3DSparseTests
    ]

superSimplex2DGridTests :: [TestTree]
superSimplex2DGridTests = golden2DImageTests "supersimplex" defaultSeeds superSimplex2

superSimplex2DSparseTests :: [TestTree]
superSimplex2DSparseTests = golden2DSparseTests "supersimplex" defaultSeeds superSimplex2

superSimplex3DGridTests :: [TestTree]
superSimplex3DGridTests = golden3DImageTests "supersimplex" defaultSeeds superSimplex3

superSimplex3DSparseTests :: [TestTree]
superSimplex3DSparseTests = golden3DSparseTests "supersimplex" defaultSeeds superSimplex3
