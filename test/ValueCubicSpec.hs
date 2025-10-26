module ValueCubicSpec (test_golden_valuecubic) where

import Golden.Util
import Numeric.Noise
import Test.Tasty (TestTree, testGroup)

test_golden_valuecubic :: TestTree
test_golden_valuecubic =
  testGroup
    "ValueCubic Golden Tests"
    [ testGroup "2D Grid Tests" valueCubic2DGridTests
    , testGroup "2D Sparse Tests" valueCubic2DSparseTests
    , testGroup "3D Grid Tests" valueCubic3DGridTests
    , testGroup "3D Sparse Tests" valueCubic3DSparseTests
    ]

valueCubic2DGridTests :: [TestTree]
valueCubic2DGridTests = golden2DImageTests "valuecubic" defaultSeeds valueCubic2

valueCubic2DSparseTests :: [TestTree]
valueCubic2DSparseTests = golden2DSparseTests "valuecubic" defaultSeeds valueCubic2

valueCubic3DGridTests :: [TestTree]
valueCubic3DGridTests = golden3DImageTests "valuecubic" defaultSeeds valueCubic3

valueCubic3DSparseTests :: [TestTree]
valueCubic3DSparseTests = golden3DSparseTests "valuecubic" defaultSeeds valueCubic3
