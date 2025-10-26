module PerlinSpec where

import GHC.Exts (noinline)
import Golden.Util
import Numeric.Noise
import Test.Tasty (TestTree, testGroup)

seed :: Seed
seed = 82384

prop_noise2_normalized :: Double -> Double -> Bool
prop_noise2_normalized x y =
  let n = noise2At perlin2 seed x y
   in n >= -1 && n <= 1

prop_noise2_addition_associative :: Rational -> Rational -> Bool
prop_noise2_addition_associative x y =
  noise2At ((noinline perlin2 + noinline superSimplex2) + noinline openSimplex2) seed x y
    == noise2At (perlin2 + (noinline superSimplex2 + noinline openSimplex2)) seed x y

prop_noise2_addition_commutative :: Rational -> Rational -> Bool
prop_noise2_addition_commutative x y =
  noise2At (noinline perlin2 + noinline superSimplex2) seed x y
    == noise2At (noinline superSimplex2 + noinline perlin2) seed x y

test_golden_perlin :: TestTree
test_golden_perlin =
  testGroup
    "Perlin Golden Tests"
    [ testGroup "2D Grid Tests" perlin2DGridTests
    , testGroup "2D Sparse Tests" perlin2DSparseTests
    , testGroup "3D Grid Tests" perlin3DGridTests
    , testGroup "3D Sparse Tests" perlin3DSparseTests
    ]

perlin2DGridTests :: [TestTree]
perlin2DGridTests = golden2DImageTests "perlin" defaultSeeds perlin2

perlin2DSparseTests :: [TestTree]
perlin2DSparseTests = golden2DSparseTests "perlin" defaultSeeds perlin2

perlin3DGridTests :: [TestTree]
perlin3DGridTests = golden3DImageTests "perlin" defaultSeeds perlin3

perlin3DSparseTests :: [TestTree]
perlin3DSparseTests = golden3DSparseTests "perlin" defaultSeeds perlin3
