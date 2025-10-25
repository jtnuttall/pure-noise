{-# OPTIONS_GHC -ddump-rule-firings #-}

import BenchLib
import Data.Typeable
import Data.Vector.Unboxed qualified as U
import Numeric.Noise
import System.Random.MWC qualified as MWC

main :: IO ()
main = do
  let sz = 1_000_000
      seed = 1337
      octaves = 8
  defaultMain
    [ bgroup
        "2D"
        ( baseline2 seed sz
            <> benchPerlin2 seed octaves sz
            <> benchOpenSimplex2 seed octaves sz
            <> benchOpenSimplexSmooth2 seed octaves sz
            <> benchValue2 seed octaves sz
            <> benchValueCubic2 seed octaves sz
            <> benchCombo2 seed octaves sz
            <> benchCellular2 seed octaves sz
        )
    , bgroup
        "3D"
        ( baseline3 seed sz
            <> benchPerlin3 seed octaves sz
            <> benchValue3 seed octaves sz
            <> benchValueCubic3 seed octaves sz
        )
    ]

label :: (Typeable a) => String -> Int -> Seed -> Proxy a -> String
label lbl sz seed px =
  let lbl' = case lbl of
        "" -> ""
        v -> v <> ": "
   in lbl' <> showsTypeRep (typeRep px) "" <> "[seed=" <> show seed <> "] x" <> show sz

createEnv2 :: forall a. (U.Unbox a, MWC.UniformRange a, RealFrac a) => Int -> IO (Seed, U.Vector (a, a))
createEnv2 sz = do
  g <- MWC.createSystemRandom
  seed <- MWC.uniformRM (minBound, maxBound) g
  v <- U.generateM sz $ \i -> do
    -- most of these functions zero at whole numbers and can short-circuit,
    -- so a random offset should give a better signal of real world performance
    offset <- MWC.uniformRM (0.00001, 0.99999) g
    pure $
      let r = fromIntegral $ i `div` (sz `div` 2)
          c = fromIntegral $ i `mod` (sz `div` 2)
       in (r + offset, c + offset)
  pure (seed, v)
{-# INLINE createEnv2 #-}

benchMany2
  :: forall a
   . (Typeable a, MWC.UniformRange a, U.Unbox a, RealFrac a)
  => String
  -> Int
  -> Seed
  -> Noise2 a
  -> Benchmark
benchMany2 lbl sz deprecatedSeed f =
  env (createEnv2 sz) $ \ ~(seed, v) ->
    bench (label lbl sz deprecatedSeed (Proxy @(U.Vector a))) $
      nf (U.map (uncurry (noise2At f seed))) v
{-# INLINE benchMany2 #-}

baseline2 :: Seed -> Int -> [Benchmark]
baseline2 seed sz =
  [ bgroup
      "baseline2"
      [ benchMany2 @Float "" sz seed (const2 1)
      , benchMany2 @Double "" sz seed (const2 2)
      ]
  ]
{-# INLINE baseline2 #-}

benchPerlin2 :: Seed -> Int -> Int -> [Benchmark]
benchPerlin2 seed octaves sz =
  [ bgroup
      "perlin2"
      [ benchMany2 @Float "" sz seed perlin2
      , benchMany2 @Double "" sz seed perlin2
      , benchMany2 @Float "fractal" sz seed (fractal2 defaultFractalConfig{octaves} perlin2)
      , benchMany2 @Double "fractal" sz seed (fractal2 defaultFractalConfig{octaves} perlin2)
      , benchMany2 @Float "ridged" sz seed (ridged2 defaultFractalConfig{octaves} perlin2)
      , benchMany2 @Double "ridged" sz seed (ridged2 defaultFractalConfig{octaves} perlin2)
      , benchMany2 @Float "billow" sz seed (billow2 defaultFractalConfig{octaves} perlin2)
      , benchMany2 @Double "billow" sz seed (billow2 defaultFractalConfig{octaves} perlin2)
      , benchMany2 @Float "pingPong" sz seed (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength perlin2)
      , benchMany2 @Double "pingPong" sz seed (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength perlin2)
      ]
  ]
{-# INLINE benchPerlin2 #-}

benchOpenSimplex2 :: Seed -> Int -> Int -> [Benchmark]
benchOpenSimplex2 seed octaves sz =
  [ bgroup
      "openSimplex2"
      [ benchMany2 @Float "" sz seed openSimplex2
      , benchMany2 @Double "" sz seed openSimplex2
      , benchMany2 @Float "fractal" sz seed (fractal2 defaultFractalConfig{octaves} openSimplex2)
      , benchMany2 @Double "fractal" sz seed (fractal2 defaultFractalConfig{octaves} openSimplex2)
      , benchMany2 @Float "ridged" sz seed (ridged2 defaultFractalConfig{octaves} openSimplex2)
      , benchMany2 @Double "ridged" sz seed (ridged2 defaultFractalConfig{octaves} openSimplex2)
      , benchMany2 @Float "billow" sz seed (billow2 defaultFractalConfig{octaves} openSimplex2)
      , benchMany2 @Double "billow" sz seed (billow2 defaultFractalConfig{octaves} openSimplex2)
      , benchMany2 @Float "pingPong" sz seed (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength openSimplex2)
      , benchMany2 @Double "pingPong" sz seed (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength openSimplex2)
      ]
  ]
{-# INLINE benchOpenSimplex2 #-}

benchOpenSimplexSmooth2 :: Seed -> Int -> Int -> [Benchmark]
benchOpenSimplexSmooth2 seed octaves sz =
  [ bgroup
      "superSimplex2"
      [ benchMany2 @Float "" sz seed superSimplex2
      , benchMany2 @Double "" sz seed superSimplex2
      , benchMany2 @Float "fractal" sz seed (fractal2 defaultFractalConfig{octaves} superSimplex2)
      , benchMany2 @Double "fractal" sz seed (fractal2 defaultFractalConfig{octaves} superSimplex2)
      , benchMany2 @Float "ridged" sz seed (ridged2 defaultFractalConfig{octaves} superSimplex2)
      , benchMany2 @Double "ridged" sz seed (ridged2 defaultFractalConfig{octaves} superSimplex2)
      , benchMany2 @Float "billow" sz seed (billow2 defaultFractalConfig{octaves} superSimplex2)
      , benchMany2 @Double "billow" sz seed (billow2 defaultFractalConfig{octaves} superSimplex2)
      , benchMany2 @Float "pingPong" sz seed (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength superSimplex2)
      , benchMany2 @Double "pingPong" sz seed (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength superSimplex2)
      ]
  ]
{-# INLINE benchOpenSimplexSmooth2 #-}

benchValue2 :: Seed -> Int -> Int -> [Benchmark]
benchValue2 seed octaves sz =
  [ bgroup
      "value2"
      [ benchMany2 @Float "" sz seed value2
      , benchMany2 @Double "" sz seed value2
      , benchMany2 @Float "fractal" sz seed (fractal2 defaultFractalConfig{octaves} value2)
      , benchMany2 @Double "fractal" sz seed (fractal2 defaultFractalConfig{octaves} value2)
      , benchMany2 @Float "ridged" sz seed (ridged2 defaultFractalConfig{octaves} value2)
      , benchMany2 @Double "ridged" sz seed (ridged2 defaultFractalConfig{octaves} value2)
      , benchMany2 @Float "billow" sz seed (billow2 defaultFractalConfig{octaves} value2)
      , benchMany2 @Double "billow" sz seed (billow2 defaultFractalConfig{octaves} value2)
      , benchMany2 @Float "pingPong" sz seed (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength value2)
      , benchMany2 @Double "pingPong" sz seed (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength value2)
      ]
  ]
{-# INLINE benchValue2 #-}

benchValueCubic2 :: Seed -> Int -> Int -> [Benchmark]
benchValueCubic2 seed octaves sz =
  [ bgroup
      "valueCubic2"
      [ benchMany2 @Float "" sz seed valueCubic2
      , benchMany2 @Double "" sz seed valueCubic2
      , benchMany2 @Float "fractal" sz seed (fractal2 defaultFractalConfig{octaves} valueCubic2)
      , benchMany2 @Double "fractal" sz seed (fractal2 defaultFractalConfig{octaves} valueCubic2)
      , benchMany2 @Float "ridged" sz seed (ridged2 defaultFractalConfig{octaves} valueCubic2)
      , benchMany2 @Double "ridged" sz seed (ridged2 defaultFractalConfig{octaves} valueCubic2)
      , benchMany2 @Float "billow" sz seed (billow2 defaultFractalConfig{octaves} valueCubic2)
      , benchMany2 @Double "billow" sz seed (billow2 defaultFractalConfig{octaves} valueCubic2)
      , benchMany2 @Float "pingPong" sz seed (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength valueCubic2)
      , benchMany2 @Double "pingPong" sz seed (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength valueCubic2)
      ]
  ]
{-# INLINE benchValueCubic2 #-}

benchCombo2 :: Seed -> Int -> Int -> [Benchmark]
benchCombo2 seed octaves sz =
  [ bgroup
      "numeric combination"
      [ benchMany2 @Float "perlin * opensimplex/2" sz seed $
          openSimplex2 / 2 * perlin2
      , benchMany2 @Float "(4 * perlin) / 4" sz seed $
          4 * perlin2 / 4
      , benchMany2 @Float "(perlin + perlin + perlin + perlin) / 4" sz seed $
          (perlin2 + perlin2 + perlin2 + perlin2) / 4
      , benchMany2 @Float "fractal (perlin * opensimplex/2)" sz seed $
          fractal2 defaultFractalConfig{octaves} (openSimplex2 / 2 * perlin2)
      , benchMany2 @Float "fractal perlin * fractal opensimplex" sz seed $
          fractal2 defaultFractalConfig{octaves} perlin2
            * fractal2 defaultFractalConfig{octaves} openSimplex2
      ]
  ]
{-# INLINE benchCombo2 #-}

benchCellular2 :: Seed -> Int -> Int -> [Benchmark]
benchCellular2 seed _ sz =
  [ bgroup
      "cellular2"
      ( benches @Float Proxy
          <> benches @Double Proxy
      )
  ]
 where
  benches
    :: forall a. (Typeable a, MWC.UniformRange a, U.Unbox a, RealFrac a, Floating a) => Proxy a -> [Benchmark]
  benches _ =
    [ benchMany2 @a (show d <> " " <> show r) sz seed (cellular2 config)
    | d <- [DistEuclidean]
    , r <- [CellValue, Distance2Add]
    , let config = defaultCellularConfig{cellularDistanceFn = d, cellularResult = r}
    ]
  {-# INLINE benches #-}
{-# INLINE benchCellular2 #-}

createEnv3 :: (U.Unbox a, Num a) => Int -> IO (Seed, U.Vector (a, a, a))
createEnv3 sz = do
  g <- MWC.createSystemRandom
  seed <- MWC.uniformRM (minBound, maxBound) g
  !ixs <- U.generateM sz $ \i ->
    pure $
      let d = sz `div` 3
          !x = fromIntegral $ i `div` d `mod` d
          !y = fromIntegral $ i `div` (d * d)
          !z = fromIntegral $ i `div` d
       in (x, y, z)
  pure (seed, ixs)
{-# INLINE createEnv3 #-}

benchMany3
  :: forall a
   . (Typeable a, RealFrac a, U.Unbox a)
  => String
  -> Int
  -> Seed
  -> Noise3 a
  -> Benchmark
benchMany3 lbl sz deprecatedSeed f =
  env (createEnv3 sz) $ \ ~(seed, v) ->
    bench (label lbl sz deprecatedSeed (Proxy @(U.Vector a))) $
      nf (U.map (\(x, y, z) -> noise3At f seed x y z)) v
{-# INLINE benchMany3 #-}

baseline3 :: Seed -> Int -> [Benchmark]
baseline3 seed sz =
  [ bgroup
      "baseline3"
      [ benchMany3 @Float "" sz seed (const3 1)
      , benchMany3 @Double "" sz seed (const3 2)
      ]
  ]
{-# INLINE baseline3 #-}

benchPerlin3 :: Seed -> Int -> Int -> [Benchmark]
benchPerlin3 seed octaves sz =
  [ bgroup
      "perlin3"
      [ benchMany3 @Float "" sz seed perlin3
      , benchMany3 @Double "" sz seed perlin3
      , benchMany3 @Float "fractal" sz seed (fractal3 defaultFractalConfig{octaves} perlin3)
      , benchMany3 @Double "fractal" sz seed (fractal3 defaultFractalConfig{octaves} perlin3)
      , benchMany3 @Float "ridged" sz seed (ridged3 defaultFractalConfig{octaves} perlin3)
      , benchMany3 @Double "ridged" sz seed (ridged3 defaultFractalConfig{octaves} perlin3)
      , benchMany3 @Float "billow" sz seed (billow3 defaultFractalConfig{octaves} perlin3)
      , benchMany3 @Double "billow" sz seed (billow3 defaultFractalConfig{octaves} perlin3)
      , benchMany3 @Float "pingPong" sz seed (pingPong3 defaultFractalConfig{octaves} defaultPingPongStrength perlin3)
      , benchMany3 @Double "pingPong" sz seed (pingPong3 defaultFractalConfig{octaves} defaultPingPongStrength perlin3)
      ]
  ]
{-# INLINE benchPerlin3 #-}

benchValue3 :: Seed -> Int -> Int -> [Benchmark]
benchValue3 seed octaves sz =
  [ bgroup
      "value3"
      [ benchMany3 @Float "" sz seed value3
      , benchMany3 @Double "" sz seed value3
      , benchMany3 @Float "fractal" sz seed (fractal3 defaultFractalConfig{octaves} value3)
      , benchMany3 @Double "fractal" sz seed (fractal3 defaultFractalConfig{octaves} value3)
      , benchMany3 @Float "ridged" sz seed (ridged3 defaultFractalConfig{octaves} value3)
      , benchMany3 @Double "ridged" sz seed (ridged3 defaultFractalConfig{octaves} value3)
      , benchMany3 @Float "billow" sz seed (billow3 defaultFractalConfig{octaves} value3)
      , benchMany3 @Double "billow" sz seed (billow3 defaultFractalConfig{octaves} value3)
      , benchMany3 @Float "pingPong" sz seed (pingPong3 defaultFractalConfig{octaves} defaultPingPongStrength value3)
      , benchMany3 @Double "pingPong" sz seed (pingPong3 defaultFractalConfig{octaves} defaultPingPongStrength value3)
      ]
  ]
{-# INLINE benchValue3 #-}

benchValueCubic3 :: Seed -> Int -> Int -> [Benchmark]
benchValueCubic3 seed octaves sz =
  [ bgroup
      "valueCubic3"
      [ benchMany3 @Float "" sz seed valueCubic3
      , benchMany3 @Double "" sz seed valueCubic3
      , benchMany3 @Float "fractal" sz seed (fractal3 defaultFractalConfig{octaves} valueCubic3)
      , benchMany3 @Double "fractal" sz seed (fractal3 defaultFractalConfig{octaves} valueCubic3)
      , benchMany3 @Float "ridged" sz seed (ridged3 defaultFractalConfig{octaves} valueCubic3)
      , benchMany3 @Double "ridged" sz seed (ridged3 defaultFractalConfig{octaves} valueCubic3)
      , benchMany3 @Float "billow" sz seed (billow3 defaultFractalConfig{octaves} valueCubic3)
      , benchMany3 @Double "billow" sz seed (billow3 defaultFractalConfig{octaves} valueCubic3)
      , benchMany3 @Float "pingPong" sz seed (pingPong3 defaultFractalConfig{octaves} defaultPingPongStrength valueCubic3)
      , benchMany3 @Double "pingPong" sz seed (pingPong3 defaultFractalConfig{octaves} defaultPingPongStrength valueCubic3)
      ]
  ]
{-# INLINE benchValueCubic3 #-}
