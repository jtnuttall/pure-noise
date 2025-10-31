{-# LANGUAGE OverloadedStrings #-}

module OpenSimplexSpec (test_golden_opensimplex) where

import Golden.Util
import Numeric.Noise
import Test.Tasty (TestTree, testGroup)

test_golden_opensimplex :: TestTree
test_golden_opensimplex =
  testGroup
    "OpenSimplex Golden Tests"
    [ testGroup "2D Grid Tests" openSimplex2DGridTests
    , testGroup "2D Sparse Tests" openSimplex2DSparseTests
    ]

openSimplex2DGridTests :: [TestTree]
openSimplex2DGridTests = golden2DImageTests "opensimplex" defaultSeeds openSimplex2

openSimplex2DSparseTests :: [TestTree]
openSimplex2DSparseTests = golden2DSparseTests "opensimplex" defaultSeeds openSimplex2
