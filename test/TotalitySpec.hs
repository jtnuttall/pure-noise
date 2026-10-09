-- | Out-of-domain inputs (beyond the Int32 lattice, non-finite) produce
-- unspecified /values/, but they must stay values: every noise function is
-- total, so one bad coordinate upstream can't crash a whole render. These
-- assertions replace the old out-of-domain golden points, which pinned the
-- exact garbage and so broke on any implementation change.
module TotalitySpec (test_totality) where

import Control.Exception (evaluate)
import Control.Monad (forM_, void)
import Numeric.Noise
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

-- Beyond the Int32 lattice, past Double integer precision, and non-finite.
badCoords :: [Double]
badCoords = [1.0e10, -1.0e10, 2 ^ (53 :: Int), 1 / 0, -1 / 0, 0 / 0]

noises2 :: [(String, Noise2 Double)]
noises2 =
  [ ("perlin2", perlin2)
  , ("openSimplex2", openSimplex2)
  , ("smootherSimplex2", smootherSimplex2)
  , ("value2", value2)
  , ("valueCubic2", valueCubic2)
  , ("cellular2", cellular2 defaultCellularConfig)
  , ("fractal2 perlin2", fractal2 defaultFractalConfig perlin2)
  ]

noises3 :: [(String, Noise3 Double)]
noises3 =
  [ ("perlin3", perlin3)
  , ("openSimplex3", openSimplex3)
  , ("smootherSimplex3", smootherSimplex3)
  , ("value3", value3)
  , ("valueCubic3", valueCubic3)
  , ("cellular3", cellular3 defaultCellularConfig)
  , ("fractal3 perlin3", fractal3 defaultFractalConfig perlin3)
  ]

test_totality :: TestTree
test_totality =
  testGroup
    "Totality on out-of-domain inputs"
    [ testGroup
        "2D"
        [ testCase name $
            forM_ badCoords $ \c ->
              forM_ [(c, 0.5), (0.5, c), (c, c)] $ \(x, y) ->
                void $ evaluate (noise2At noise 0 x y)
        | (name, noise) <- noises2
        ]
    , testGroup
        "3D"
        [ testCase name $
            forM_ badCoords $ \c ->
              forM_ [(c, 0.5, 0.5), (0.5, c, 0.5), (0.5, 0.5, c), (c, c, c)] $ \(x, y, z) ->
                void $ evaluate (noise3At noise 0 x y z)
        | (name, noise) <- noises3
        ]
    ]
