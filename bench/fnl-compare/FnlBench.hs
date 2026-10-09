-- | Performance comparison against the vendored FastNoiseLite
--
-- Ratios are printed by tasty-bench's 'bcompare': each @pure-noise@ line ends
-- with e.g. @1.05x@ = its mean time relative to the matching @fnl@ line
-- (lower is better; 1.05x means pure-noise is 5% slower).
--
-- Ratios only print when the matching fnl benchmark runs in the same invocation,
-- so filter by group (e.g. @-p \/perlin\/@), not by side.
module Main (main) where

import Control.Monad.State.Strict (evalState, state)
import Data.Vector.Storable qualified as S
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.Ptr (Ptr)
import Numeric.Noise
import System.Random.SplitMix (SMGen, mkSMGen, nextFloat)
import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer (printAwkExpr)

main :: IO ()
main =
  defaultMain (map (mapLeafBenchmarks addCompare) benchmarks)

-- | Attach a bcompare ratio to every pure-noise leaf, targeting the fnl leaf
-- in the same group (the pattern matches exactly one benchmark).
addCompare :: [String] -> Benchmark -> Benchmark
addCompare (leaf : path) b
  | leaf == pnLeaf = bcompare (printAwkExpr (locateBenchmark (fnlLeaf : path))) b
addCompare _ b = b

fnlLeaf, pnLeaf :: String
fnlLeaf = "fnl x" <> show samples
pnLeaf = "pure-noise x" <> show samples

benchmarks :: [Benchmark]
benchmarks =
  [ env mkEnv2 $ \ ~(grid, rand, kOne, kSmall) ->
      bgroup
        "2D"
        [ algo2 grid rand kOne kSmall "value" fnlValue value2
        , algo2 grid rand kOne kSmall "valueCubic" fnlValueCubic valueCubic2
        , algo2 grid rand kOne kSmall "perlin" fnlPerlin perlin2
        , algo2 grid rand kOne kSmall "openSimplex2" fnlOpenSimplex2 openSimplex2
        , algo2 grid rand kOne kSmall "smootherSimplex2" fnlOpenSimplex2S smootherSimplex2
        , algo2 grid rand kOne kSmall "cellular" fnlCellular (cellular2 benchCellularConfig)
        ]
  , env mkEnv3 $ \ ~(grid, rand, kOne, kSmall) ->
      bgroup
        "3D"
        [ algo3 grid rand kOne kSmall "value" fnlValue value3
        , algo3 grid rand kOne kSmall "valueCubic" fnlValueCubic valueCubic3
        , algo3 grid rand kOne kSmall "perlin" fnlPerlin perlin3
        , algo3 grid rand kOne kSmall "openSimplex2" fnlOpenSimplex2 openSimplex3
        , algo3 grid rand kOne kSmall "smootherSimplex2" fnlOpenSimplex2S smootherSimplex3
        , algo3 grid rand kOne kSmall "cellular" fnlCellular (cellular3 benchCellularConfig)
        ]
  ]

algo2
  :: (S.Vector Float, S.Vector Float)
  -> (S.Vector Float, S.Vector Float)
  -> Float
  -> Float
  -> String
  -> CInt
  -> Noise2 Float
  -> Benchmark
algo2 grid rand kOne kSmall name ty nz =
  bgroup
    name
    [ bgroup "grid" (pair grid kOne)
    , bgroup "freq001" (pair grid kSmall)
    , bgroup "random" (pair rand kOne)
    ]
 where
  pair ~(xs, ys) k =
    [ bench fnlLeaf $ whnfIO (fnlSum2 ty k xs ys)
    , bench pnLeaf $ whnf (\kk -> sumNoise2 nz kk xs ys) k
    ]
{-# INLINE algo2 #-}

algo3
  :: (S.Vector Float, S.Vector Float, S.Vector Float)
  -> (S.Vector Float, S.Vector Float, S.Vector Float)
  -> Float
  -> Float
  -> String
  -> CInt
  -> Noise3 Float
  -> Benchmark
algo3 grid rand kOne kSmall name ty nz =
  bgroup
    name
    [ bgroup "grid" (pair grid kOne)
    , bgroup "freq001" (pair grid kSmall)
    , bgroup "random" (pair rand kOne)
    ]
 where
  -- lazy pattern: see algo2
  pair ~(xs, ys, zs) k =
    [ bench fnlLeaf $ whnfIO (fnlSum3 ty k xs ys zs)
    , bench pnLeaf $ whnf (\kk -> sumNoise3 nz kk xs ys zs) k
    ]
{-# INLINE algo3 #-}

----------------------------------------------------------------------------------------------------
-- Matched sweeps
----------------------------------------------------------------------------------------------------

sumNoise2 :: Noise2 Float -> Float -> S.Vector Float -> S.Vector Float -> Float
sumNoise2 nz k xs ys = go 0 0
 where
  !n = S.length xs
  go !i !acc
    | i >= n = acc
    | otherwise =
        go (i + 1) (acc + noise2At nz benchSeed (k * S.unsafeIndex xs i) (k * S.unsafeIndex ys i))
{-# INLINE sumNoise2 #-}

sumNoise3 :: Noise3 Float -> Float -> S.Vector Float -> S.Vector Float -> S.Vector Float -> Float
sumNoise3 nz k xs ys zs = go 0 0
 where
  !n = S.length xs
  go !i !acc
    | i >= n = acc
    | otherwise =
        go
          (i + 1)
          (acc + noise3At nz benchSeed (k * S.unsafeIndex xs i) (k * S.unsafeIndex ys i) (k * S.unsafeIndex zs i))
{-# INLINE sumNoise3 #-}

----------------------------------------------------------------------------------------------------
-- FFI (fnl_bench_shim.cpp)
----------------------------------------------------------------------------------------------------

foreign import ccall unsafe "fnl_bench_sum2"
  c_fnlSum2 :: CInt -> CInt -> CFloat -> CInt -> Ptr Float -> Ptr Float -> IO CFloat

foreign import ccall unsafe "fnl_bench_sum3"
  c_fnlSum3 :: CInt -> CInt -> CFloat -> CInt -> Ptr Float -> Ptr Float -> Ptr Float -> IO CFloat

fnlSum2 :: CInt -> Float -> S.Vector Float -> S.Vector Float -> IO Float
fnlSum2 ty k xs ys =
  S.unsafeWith xs $ \px ->
    S.unsafeWith ys $ \py -> do
      CFloat r <- c_fnlSum2 ty fnlBenchSeed (CFloat k) (fromIntegral (S.length xs)) px py
      pure r

fnlSum3 :: CInt -> Float -> S.Vector Float -> S.Vector Float -> S.Vector Float -> IO Float
fnlSum3 ty k xs ys zs =
  S.unsafeWith xs $ \px ->
    S.unsafeWith ys $ \py ->
      S.unsafeWith zs $ \pz -> do
        CFloat r <- c_fnlSum3 ty fnlBenchSeed (CFloat k) (fromIntegral (S.length xs)) px py pz
        pure r

fnlOpenSimplex2, fnlOpenSimplex2S, fnlCellular, fnlPerlin, fnlValueCubic, fnlValue :: CInt
fnlOpenSimplex2 = 0
fnlOpenSimplex2S = 1
fnlCellular = 2
fnlPerlin = 3
fnlValueCubic = 4
fnlValue = 5

-- 1337 is the FNL bench convention.
benchSeed :: Seed
benchSeed = 1337

fnlBenchSeed :: CInt
fnlBenchSeed = 1337

benchCellularConfig :: CellularConfig Float
benchCellularConfig = defaultCellularConfig{cellularDistanceFn = DistEuclidean, cellularResult = Distance}

-- ---------------------------------------------------------------------------
-- Coordinate environments (built once per dimension)
-- ---------------------------------------------------------------------------

-- n = 262144 everywhere: 512*512 (2D grid), 64^3 (3D grid), and the random
-- streams, so every leaf carries the same "xN" count for vps.py.
gridSize2, gridSize3, samples :: Int
gridSize2 = 512
gridSize3 = 64
samples = gridSize2 * gridSize2

-- (grid xs/ys, random xs/ys, kOne = 1.0, kSmall = 0.01). The ks come out of
-- the IO action so the simplifier cannot constant-fold the per-point multiply
-- on the Haskell side only.
mkEnv2 :: IO ((S.Vector Float, S.Vector Float), (S.Vector Float, S.Vector Float), Float, Float)
mkEnv2 = pure (mkGrid2, mkRand2 samples, 1.0, 0.01)

mkEnv3
  :: IO
       ( (S.Vector Float, S.Vector Float, S.Vector Float)
       , (S.Vector Float, S.Vector Float, S.Vector Float)
       , Float
       , Float
       )
mkEnv3 = pure (mkGrid3, mkRand3 samples, 1.0, 0.01)

-- Integer lattice in FNL benchmark loop order (x fastest, then y, then z).
mkGrid2 :: (S.Vector Float, S.Vector Float)
mkGrid2 =
  ( S.generate samples (\i -> fromIntegral (i `mod` gridSize2))
  , S.generate samples (\i -> fromIntegral (i `div` gridSize2))
  )

mkGrid3 :: (S.Vector Float, S.Vector Float, S.Vector Float)
mkGrid3 =
  ( S.generate samples (\i -> fromIntegral (i `mod` gridSize3))
  , S.generate samples (\i -> fromIntegral ((i `div` gridSize3) `mod` gridSize3))
  , S.generate samples (\i -> fromIntegral (i `div` (gridSize3 * gridSize3)))
  )

smgen :: SMGen
smgen = mkSMGen 0x9E3779B97F4A7C15

draws :: Int -> S.Vector Float
draws n = S.map (* 512) . evalState (S.generateM n . const . state $ nextFloat) $ smgen

mkRand2 :: Int -> (S.Vector Float, S.Vector Float)
mkRand2 n =
  let v = draws (2 * n)
   in ( S.generate n (\i -> v S.! (2 * i))
      , S.generate n (\i -> v S.! (2 * i + 1))
      )

mkRand3 :: Int -> (S.Vector Float, S.Vector Float, S.Vector Float)
mkRand3 n =
  let v = draws (3 * n)
   in ( S.generate n (\i -> v S.! (3 * i))
      , S.generate n (\i -> v S.! (3 * i + 1))
      , S.generate n (\i -> v S.! (3 * i + 2))
      )
