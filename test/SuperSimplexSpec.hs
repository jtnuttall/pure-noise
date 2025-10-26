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
    ]

superSimplex2DGridTests :: [TestTree]
superSimplex2DGridTests = golden2DImageTests "supersimplex" defaultSeeds superSimplex2

superSimplex2DSparseTests :: [TestTree]
superSimplex2DSparseTests = golden2DSparseTests "supersimplex" defaultSeeds superSimplex2
