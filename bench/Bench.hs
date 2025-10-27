import BenchLib
import Data.Massiv.Array qualified as MA
import Data.Typeable
import Data.Vector.Unboxed qualified as U
import Numeric.Noise
import System.Random.Stateful

main :: IO ()
main = do
  let sz = 1_000_000
      octaves = 8
      massivW = 1_000
      massivH = 1_000
  defaultMain
    [ bgroup
        "2D"
        ( baseline2 sz
            <> benchPerlin2 octaves sz
            <> benchOpenSimplex2 octaves sz
            <> benchOpenSimplexSmooth2 octaves sz
            <> benchValue2 octaves sz
            <> benchValueCubic2 octaves sz
            <> benchCombo2 octaves sz
            <> benchCellular2 octaves sz
        )
    , bgroup
        "3D"
        ( baseline3 sz
            <> benchPerlin3 octaves sz
            <> benchValue3 octaves sz
            <> benchValueCubic3 octaves sz
        )
    , bgroup
        "2D massiv"
        ( benchMassivBase2 massivW massivH
            <> benchMassivFractal2 octaves massivW massivH
        )
    ]

label :: (Typeable a) => String -> Int -> Proxy a -> String
label lbl sz px =
  let lbl' = case lbl of
        "" -> ""
        v -> v <> ": "
   in lbl' <> showsTypeRep (typeRep px) "" <> " x" <> show sz

-- most of these functions zero at whole numbers and can short-circuit,
-- so a random offset should give a better signal of real world performance
generate2DCoords
  :: ( UniformRange a
     , RealFrac a
     , StatefulGen g IO
     )
  => g
  -> Int
  -> Int
  -> IO (a, a)
generate2DCoords g i j = do
  offsetX <- uniformRM (0.00001, 0.99999) g
  offsetY <- uniformRM (0.00001, 0.99999) g
  let r = fromIntegral i
      c = fromIntegral j

  pure (r + offsetX, c + offsetY)
{-# INLINE generate2DCoords #-}

createEnv2 :: forall a. (U.Unbox a, UniformRange a, RealFrac a) => Int -> IO (Seed, U.Vector (a, a))
createEnv2 sz = do
  g <- newAtomicGenM =<< newStdGen
  seed <- uniformRM (minBound, maxBound) g
  v <- U.generateM sz $ \i ->
    generate2DCoords
      g
      (i `div` (sz `div` 2))
      (i `mod` (sz `div` 2))
  pure (seed, v)
{-# INLINE createEnv2 #-}

benchMany2
  :: forall a
   . (Typeable a, UniformRange a, U.Unbox a, RealFrac a)
  => String
  -> Int
  -> Noise2 a
  -> Benchmark
benchMany2 lbl sz f =
  env (createEnv2 sz) $ \ ~(seed, v) ->
    bench (label lbl sz (Proxy @(U.Vector a))) $
      nf (U.map (uncurry (noise2At f seed))) v
{-# INLINE benchMany2 #-}

baseline2 :: Int -> [Benchmark]
baseline2 sz =
  [ bgroup
      "baseline2"
      [ benchMany2 @Float "" sz (const2 1)
      , benchMany2 @Double "" sz (const2 2)
      ]
  ]

benchPerlin2 :: Int -> Int -> [Benchmark]
benchPerlin2 octaves sz =
  [ bgroup
      "perlin2"
      [ benchMany2 @Float "" sz perlin2
      , benchMany2 @Double "" sz perlin2
      , benchMany2 @Float "fractal" sz (fractal2 defaultFractalConfig{octaves} perlin2)
      , benchMany2 @Double "fractal" sz (fractal2 defaultFractalConfig{octaves} perlin2)
      , benchMany2 @Float "ridged" sz (ridged2 defaultFractalConfig{octaves} perlin2)
      , benchMany2 @Double "ridged" sz (ridged2 defaultFractalConfig{octaves} perlin2)
      , benchMany2 @Float "billow" sz (billow2 defaultFractalConfig{octaves} perlin2)
      , benchMany2 @Double "billow" sz (billow2 defaultFractalConfig{octaves} perlin2)
      , benchMany2 @Float "pingPong" sz (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength perlin2)
      , benchMany2 @Double "pingPong" sz (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength perlin2)
      ]
  ]

benchOpenSimplex2 :: Int -> Int -> [Benchmark]
benchOpenSimplex2 octaves sz =
  [ bgroup
      "openSimplex2"
      [ benchMany2 @Float "" sz openSimplex2
      , benchMany2 @Double "" sz openSimplex2
      , benchMany2 @Float "fractal" sz (fractal2 defaultFractalConfig{octaves} openSimplex2)
      , benchMany2 @Double "fractal" sz (fractal2 defaultFractalConfig{octaves} openSimplex2)
      , benchMany2 @Float "ridged" sz (ridged2 defaultFractalConfig{octaves} openSimplex2)
      , benchMany2 @Double "ridged" sz (ridged2 defaultFractalConfig{octaves} openSimplex2)
      , benchMany2 @Float "billow" sz (billow2 defaultFractalConfig{octaves} openSimplex2)
      , benchMany2 @Double "billow" sz (billow2 defaultFractalConfig{octaves} openSimplex2)
      , benchMany2 @Float "pingPong" sz (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength openSimplex2)
      , benchMany2 @Double "pingPong" sz (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength openSimplex2)
      ]
  ]

benchOpenSimplexSmooth2 :: Int -> Int -> [Benchmark]
benchOpenSimplexSmooth2 octaves sz =
  [ bgroup
      "superSimplex2"
      [ benchMany2 @Float "" sz superSimplex2
      , benchMany2 @Double "" sz superSimplex2
      , benchMany2 @Float "fractal" sz (fractal2 defaultFractalConfig{octaves} superSimplex2)
      , benchMany2 @Double "fractal" sz (fractal2 defaultFractalConfig{octaves} superSimplex2)
      , benchMany2 @Float "ridged" sz (ridged2 defaultFractalConfig{octaves} superSimplex2)
      , benchMany2 @Double "ridged" sz (ridged2 defaultFractalConfig{octaves} superSimplex2)
      , benchMany2 @Float "billow" sz (billow2 defaultFractalConfig{octaves} superSimplex2)
      , benchMany2 @Double "billow" sz (billow2 defaultFractalConfig{octaves} superSimplex2)
      , benchMany2 @Float "pingPong" sz (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength superSimplex2)
      , benchMany2 @Double "pingPong" sz (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength superSimplex2)
      ]
  ]

benchValue2 :: Int -> Int -> [Benchmark]
benchValue2 octaves sz =
  [ bgroup
      "value2"
      [ benchMany2 @Float "" sz value2
      , benchMany2 @Double "" sz value2
      , benchMany2 @Float "fractal" sz (fractal2 defaultFractalConfig{octaves} value2)
      , benchMany2 @Double "fractal" sz (fractal2 defaultFractalConfig{octaves} value2)
      , benchMany2 @Float "ridged" sz (ridged2 defaultFractalConfig{octaves} value2)
      , benchMany2 @Double "ridged" sz (ridged2 defaultFractalConfig{octaves} value2)
      , benchMany2 @Float "billow" sz (billow2 defaultFractalConfig{octaves} value2)
      , benchMany2 @Double "billow" sz (billow2 defaultFractalConfig{octaves} value2)
      , benchMany2 @Float "pingPong" sz (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength value2)
      , benchMany2 @Double "pingPong" sz (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength value2)
      ]
  ]

benchValueCubic2 :: Int -> Int -> [Benchmark]
benchValueCubic2 octaves sz =
  [ bgroup
      "valueCubic2"
      [ benchMany2 @Float "" sz valueCubic2
      , benchMany2 @Double "" sz valueCubic2
      , benchMany2 @Float "fractal" sz (fractal2 defaultFractalConfig{octaves} valueCubic2)
      , benchMany2 @Double "fractal" sz (fractal2 defaultFractalConfig{octaves} valueCubic2)
      , benchMany2 @Float "ridged" sz (ridged2 defaultFractalConfig{octaves} valueCubic2)
      , benchMany2 @Double "ridged" sz (ridged2 defaultFractalConfig{octaves} valueCubic2)
      , benchMany2 @Float "billow" sz (billow2 defaultFractalConfig{octaves} valueCubic2)
      , benchMany2 @Double "billow" sz (billow2 defaultFractalConfig{octaves} valueCubic2)
      , benchMany2 @Float "pingPong" sz (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength valueCubic2)
      , benchMany2 @Double "pingPong" sz (pingPong2 defaultFractalConfig{octaves} defaultPingPongStrength valueCubic2)
      ]
  ]

benchCombo2 :: Int -> Int -> [Benchmark]
benchCombo2 octaves sz =
  [ bgroup
      "numeric combination"
      [ benchMany2 @Float "perlin * opensimplex/2" sz $
          openSimplex2 / 2 * perlin2
      , benchMany2 @Float "(4 * perlin) / 4" sz $
          4 * perlin2 / 4
      , benchMany2 @Float "(perlin + perlin + perlin + perlin) / 4" sz $
          (perlin2 + perlin2 + perlin2 + perlin2) / 4
      , benchMany2 @Float "fractal (perlin * opensimplex/2)" sz $
          fractal2 defaultFractalConfig{octaves} (openSimplex2 / 2 * perlin2)
      , benchMany2 @Float "fractal perlin * fractal opensimplex" sz $
          fractal2 defaultFractalConfig{octaves} perlin2
            * fractal2 defaultFractalConfig{octaves} openSimplex2
      ]
  ]

benchCellular2 :: Int -> Int -> [Benchmark]
benchCellular2 _ sz =
  [ bgroup
      "cellular2"
      [ benchMany2 @Float
          "DistEuclidean CellValue"
          sz
          (cellular2 defaultCellularConfig{cellularDistanceFn = DistEuclidean, cellularResult = CellValue})
      , benchMany2 @Float
          "DistEuclidean Distance2Add"
          sz
          (cellular2 defaultCellularConfig{cellularDistanceFn = DistEuclidean, cellularResult = Distance2Add})
      , benchMany2 @Float
          "DistManhattan CellValue"
          sz
          (cellular2 defaultCellularConfig{cellularDistanceFn = DistManhattan, cellularResult = CellValue})
      , benchMany2 @Float
          "DistManhattan Distance2Add"
          sz
          (cellular2 defaultCellularConfig{cellularDistanceFn = DistManhattan, cellularResult = Distance2Add})
      , benchMany2 @Double
          "DistEuclidean CellValue"
          sz
          (cellular2 defaultCellularConfig{cellularDistanceFn = DistEuclidean, cellularResult = CellValue})
      , benchMany2 @Double
          "DistEuclidean Distance2Add"
          sz
          (cellular2 defaultCellularConfig{cellularDistanceFn = DistEuclidean, cellularResult = Distance2Add})
      , benchMany2 @Double
          "DistManhattan CellValue"
          sz
          (cellular2 defaultCellularConfig{cellularDistanceFn = DistManhattan, cellularResult = CellValue})
      , benchMany2 @Double
          "DistManhattan Distance2Add"
          sz
          (cellular2 defaultCellularConfig{cellularDistanceFn = DistManhattan, cellularResult = Distance2Add})
      ]
  ]

createEnv3 :: (U.Unbox a, UniformRange a, RealFrac a) => Int -> IO (Seed, U.Vector (a, a, a))
createEnv3 sz = do
  g <- newAtomicGenM =<< newStdGen
  seed <- uniformRM (minBound, maxBound) g
  offsetX <- uniformRM (0.00001, 0.99999) g
  offsetY <- uniformRM (0.00001, 0.99999) g
  offsetZ <- uniformRM (0.00001, 0.99999) g
  !ixs <- U.generateM sz $ \i -> do
    let d = sz `div` 3
        !x = fromIntegral $ i `div` d `mod` d
        !y = fromIntegral $ i `div` (d * d)
        !z = fromIntegral $ i `div` d
    pure (x + offsetX, y + offsetY, z + offsetZ)
  pure (seed, ixs)
{-# INLINE createEnv3 #-}

benchMany3
  :: forall a
   . (Typeable a, UniformRange a, RealFrac a, U.Unbox a)
  => String
  -> Int
  -> Noise3 a
  -> Benchmark
benchMany3 lbl sz f =
  env (createEnv3 sz) $ \ ~(seed, v) ->
    bench (label lbl sz (Proxy @(U.Vector a))) $
      nf (U.map (\(x, y, z) -> noise3At f seed x y z)) v
{-# INLINE benchMany3 #-}

baseline3 :: Int -> [Benchmark]
baseline3 sz =
  [ bgroup
      "baseline3"
      [ benchMany3 @Float "" sz (const3 1)
      , benchMany3 @Double "" sz (const3 2)
      ]
  ]

benchPerlin3 :: Int -> Int -> [Benchmark]
benchPerlin3 octaves sz =
  [ bgroup
      "perlin3"
      [ benchMany3 @Float "" sz perlin3
      , benchMany3 @Double "" sz perlin3
      , benchMany3 @Float "fractal" sz (fractal3 defaultFractalConfig{octaves} perlin3)
      , benchMany3 @Double "fractal" sz (fractal3 defaultFractalConfig{octaves} perlin3)
      , benchMany3 @Float "ridged" sz (ridged3 defaultFractalConfig{octaves} perlin3)
      , benchMany3 @Double "ridged" sz (ridged3 defaultFractalConfig{octaves} perlin3)
      , benchMany3 @Float "billow" sz (billow3 defaultFractalConfig{octaves} perlin3)
      , benchMany3 @Double "billow" sz (billow3 defaultFractalConfig{octaves} perlin3)
      , benchMany3 @Float "pingPong" sz (pingPong3 defaultFractalConfig{octaves} defaultPingPongStrength perlin3)
      , benchMany3 @Double "pingPong" sz (pingPong3 defaultFractalConfig{octaves} defaultPingPongStrength perlin3)
      ]
  ]

benchValue3 :: Int -> Int -> [Benchmark]
benchValue3 octaves sz =
  [ bgroup
      "value3"
      [ benchMany3 @Float "" sz value3
      , benchMany3 @Double "" sz value3
      , benchMany3 @Float "fractal" sz (fractal3 defaultFractalConfig{octaves} value3)
      , benchMany3 @Double "fractal" sz (fractal3 defaultFractalConfig{octaves} value3)
      , benchMany3 @Float "ridged" sz (ridged3 defaultFractalConfig{octaves} value3)
      , benchMany3 @Double "ridged" sz (ridged3 defaultFractalConfig{octaves} value3)
      , benchMany3 @Float "billow" sz (billow3 defaultFractalConfig{octaves} value3)
      , benchMany3 @Double "billow" sz (billow3 defaultFractalConfig{octaves} value3)
      , benchMany3 @Float "pingPong" sz (pingPong3 defaultFractalConfig{octaves} defaultPingPongStrength value3)
      , benchMany3 @Double "pingPong" sz (pingPong3 defaultFractalConfig{octaves} defaultPingPongStrength value3)
      ]
  ]

benchValueCubic3 :: Int -> Int -> [Benchmark]
benchValueCubic3 octaves sz =
  [ bgroup
      "valueCubic3"
      [ benchMany3 @Float "" sz valueCubic3
      , benchMany3 @Double "" sz valueCubic3
      , benchMany3 @Float "fractal" sz (fractal3 defaultFractalConfig{octaves} valueCubic3)
      , benchMany3 @Double "fractal" sz (fractal3 defaultFractalConfig{octaves} valueCubic3)
      , benchMany3 @Float "ridged" sz (ridged3 defaultFractalConfig{octaves} valueCubic3)
      , benchMany3 @Double "ridged" sz (ridged3 defaultFractalConfig{octaves} valueCubic3)
      , benchMany3 @Float "billow" sz (billow3 defaultFractalConfig{octaves} valueCubic3)
      , benchMany3 @Double "billow" sz (billow3 defaultFractalConfig{octaves} valueCubic3)
      , benchMany3 @Float "pingPong" sz (pingPong3 defaultFractalConfig{octaves} defaultPingPongStrength valueCubic3)
      , benchMany3 @Double "pingPong" sz (pingPong3 defaultFractalConfig{octaves} defaultPingPongStrength valueCubic3)
      ]
  ]

benchMassiv2
  :: forall a
   . (Typeable a, U.Unbox a, RealFrac a, UniformRange a)
  => String
  -> Int
  -> Int
  -> Noise2 a
  -> Benchmark
benchMassiv2 lbl !w !h noiseF =
  env
    ( do
        g <- newAtomicGenM =<< newStdGen
        seed <- uniformRM (minBound, maxBound) g
        -- This intentionally breaks the optimizer's ability to DCE. If we try to calculate
        -- inline it seems that GHC is smart enough to figure out that the whole-number
        -- integral points involve relatively simple code paths.
        (arr :: MA.Array MA.U MA.Ix2 (a, a)) <-
          MA.generateArray @MA.U MA.Par (MA.Sz2 h w) $ \(i MA.:. j) ->
            generate2DCoords g i j

        pure (seed, arr)
    )
    $ \ ~(seed, arr) ->
      bench (label lbl (w * h) (Proxy @a)) $
        nf
          ( MA.computeP @MA.U
              . MA.map
                ( \(!x, !y) ->
                    noise2At noiseF seed x y
                )
          )
          arr
{-# INLINE benchMassiv2 #-}

benchMassivBase2 :: Int -> Int -> [Benchmark]
benchMassivBase2 !w !h =
  [ bgroup
      "massiv base2"
      [ benchMassiv2 @Float "perlin2" w h perlin2
      , benchMassiv2 @Double "perlin2" w h perlin2
      , benchMassiv2 @Float "openSimplex2" w h openSimplex2
      , benchMassiv2 @Double "openSimplex2" w h openSimplex2
      , benchMassiv2 @Float "superSimplex2" w h superSimplex2
      , benchMassiv2 @Double "superSimplex2" w h superSimplex2
      , benchMassiv2 @Float "value2" w h value2
      , benchMassiv2 @Double "value2" w h value2
      , benchMassiv2 @Float "valueCubic2" w h valueCubic2
      , benchMassiv2 @Double "valueCubic2" w h valueCubic2
      , benchMassiv2 @Float
          "cellular2"
          w
          h
          (cellular2 defaultCellularConfig{cellularDistanceFn = DistEuclidean, cellularResult = CellValue})
      , benchMassiv2 @Double
          "cellular2"
          w
          h
          (cellular2 defaultCellularConfig{cellularDistanceFn = DistEuclidean, cellularResult = CellValue})
      ]
  ]

benchMassivFractal2 :: Int -> Int -> Int -> [Benchmark]
benchMassivFractal2 octaves w h =
  [ bgroup
      "massiv fractal2"
      [ benchMassiv2 @Float "perlin2 fractal" w h (fractal2 defaultFractalConfig{octaves} perlin2)
      , benchMassiv2 @Double "perlin2 fractal" w h (fractal2 defaultFractalConfig{octaves} perlin2)
      , benchMassiv2 @Float "value2 fractal" w h (fractal2 defaultFractalConfig{octaves} value2)
      , benchMassiv2 @Double "value2 fractal" w h (fractal2 defaultFractalConfig{octaves} value2)
      , benchMassiv2 @Float "openSimplex2 fractal" w h (fractal2 defaultFractalConfig{octaves} openSimplex2)
      , benchMassiv2 @Double "openSimplex2 fractal" w h (fractal2 defaultFractalConfig{octaves} openSimplex2)
      , benchMassiv2 @Float "superSimplex2 fractal" w h (fractal2 defaultFractalConfig{octaves} superSimplex2)
      , benchMassiv2 @Double "superSimplex2 fractal" w h (fractal2 defaultFractalConfig{octaves} superSimplex2)
      ]
  ]
