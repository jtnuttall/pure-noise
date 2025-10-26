{-# LANGUAGE OverloadedStrings #-}

module ValueSpec (test_golden_value) where

import Golden.Util
import Numeric.Noise
import Test.Tasty (TestTree, testGroup)

test_golden_value :: TestTree
test_golden_value =
  testGroup
    "Value Golden Tests"
    [ testGroup "2D Grid Tests" value2DGridTests
    , testGroup "2D Sparse Tests" value2DSparseTests
    , testGroup "3D Grid Tests" value3DGridTests
    , testGroup "3D Sparse Tests" value3DSparseTests
    ]

value2DGridTests :: [TestTree]
value2DGridTests = golden2DImageTests "value" defaultSeeds value2

value2DSparseTests :: [TestTree]
value2DSparseTests = golden2DSparseTests "value" defaultSeeds value2

value3DGridTests :: [TestTree]
value3DGridTests = golden3DImageTests "value" defaultSeeds value3

value3DSparseTests :: [TestTree]
value3DSparseTests = golden3DSparseTests "value" defaultSeeds value3
