{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

-- |
-- Maintainer: Jeremy Nuttall <jeremy@jeremy-nuttall.com>
-- Stability : experimental
module Numeric.Noise.Fractal (
  -- * Configuration
  FractalConfig (..),
  defaultFractalConfig,
  PingPongStrength (..),
  defaultPingPongStrength,

  -- * 2D Noise
  fractal2,
  billow2,
  ridged2,
  pingPong2,

  -- * 3D Noise
  fractal3,
  billow3,
  ridged3,
  pingPong3,

  -- * Utility
  fractalNoiseMod,
  fractalAmpMod,
  billowNoiseMod,
  billowAmpMod,
  ridgedNoiseMod,
  ridgedAmpMod,
  pingPongNoiseMod,
  pingPongAmpMod,
) where

import GHC.Generics
import Numeric.Noise.Internal

data FractalConfig a = FractalConfig
  { octaves :: Int
  , lacunarity :: a
  , gain :: a
  , weightedStrength :: a
  }
  deriving (Generic, Read, Show, Eq)

defaultFractalConfig :: (RealFrac a) => FractalConfig a
defaultFractalConfig =
  FractalConfig
    { octaves = 7
    , lacunarity = 2
    , gain = 0.5
    , weightedStrength = 0
    }

fractal2 :: (RealFrac a) => FractalConfig a -> Noise2 a -> Noise2 a
fractal2 config = Noise2 . fractal2With fractalNoiseMod (fractalAmpMod config) config . unNoise2
{-# INLINE fractal2 #-}

billow2 :: (RealFrac a) => FractalConfig a -> Noise2 a -> Noise2 a
billow2 config = Noise2 . fractal2With billowNoiseMod (billowAmpMod config) config . unNoise2
{-# INLINE billow2 #-}

ridged2 :: (RealFrac a) => FractalConfig a -> Noise2 a -> Noise2 a
ridged2 config = Noise2 . fractal2With ridgedNoiseMod (ridgedAmpMod config) config . unNoise2
{-# INLINE ridged2 #-}

pingPong2 :: (RealFrac a) => FractalConfig a -> PingPongStrength a -> Noise2 a -> Noise2 a
pingPong2 config strength =
  Noise2 . fractal2With (pingPongNoiseMod strength) (pingPongAmpMod config) config . unNoise2
{-# INLINE pingPong2 #-}

fractal2With
  :: (RealFrac a)
  => (a -> a)
  -- ^ modify noise before summation
  -> (a -> a)
  -- ^ modify amplitude
  -> FractalConfig a
  -> (Seed -> a -> a -> a)
  -> Seed
  -> a
  -> a
  -> a
fractal2With modNoise modAmps FractalConfig{..} noise2 seed x y
  | octaves < 1 = error "octaves must be a positive integer"
  | otherwise =
      let bounding = fractalBounding FractalConfig{..}
       in go octaves 0 seed 1 bounding
 where
  go 0 acc _ _ _ = acc
  go o acc s freq amp =
    let noise = amp * modNoise (noise2 s (freq * x) (freq * y))
        amp' = amp * gain * modAmps (min (noise + 1) 2)
     in go (o - 1) (acc + noise) (s + 1) (freq * lacunarity) amp'
{-# INLINE fractal2With #-}

fractal3 :: (RealFrac a) => FractalConfig a -> Noise3 a -> Noise3 a
fractal3 config = Noise3 . fractal3With fractalNoiseMod (fractalAmpMod config) config . unNoise3
{-# INLINE fractal3 #-}

billow3 :: (RealFrac a) => FractalConfig a -> Noise3 a -> Noise3 a
billow3 config = Noise3 . fractal3With billowNoiseMod (billowAmpMod config) config . unNoise3
{-# INLINE billow3 #-}

ridged3 :: (RealFrac a) => FractalConfig a -> Noise3 a -> Noise3 a
ridged3 config = Noise3 . fractal3With ridgedNoiseMod (ridgedAmpMod config) config . unNoise3
{-# INLINE ridged3 #-}

pingPong3 :: (RealFrac a) => FractalConfig a -> PingPongStrength a -> Noise3 a -> Noise3 a
pingPong3 config strength =
  Noise3 . fractal3With (pingPongNoiseMod strength) (pingPongAmpMod config) config . unNoise3
{-# INLINE pingPong3 #-}

fractal3With
  :: (RealFrac a)
  => (a -> a)
  -- ^ modify noise before summation
  -> (a -> a)
  -- ^ modify amplitude
  -> FractalConfig a
  -> (Seed -> a -> a -> a -> a)
  -> Seed
  -> a
  -> a
  -> a
  -> a
fractal3With modNoise modAmps FractalConfig{..} noise3 seed x y z
  | octaves < 1 = error "octaves must be a positive integer"
  | otherwise =
      let bounding = fractalBounding FractalConfig{..}
       in go octaves 0 seed 1 bounding
 where
  go 0 acc _ _ _ = acc
  go o acc s freq amp =
    let noise = amp * modNoise (noise3 s (freq * x) (freq * y) (freq * z))
        amp' = amp * gain * modAmps (min (noise + 1) 2)
     in go (o - 1) (acc + noise) (s + 1) (freq * lacunarity) amp'
{-# INLINE fractal3With #-}

fractalBounding :: (RealFrac a) => FractalConfig a -> a
fractalBounding FractalConfig{..} =
  let amps = take octaves $ iterate (* gain) gain
   in 1 / (sum amps + 1)
{-# INLINE fractalBounding #-}

fractalNoiseMod :: a -> a
fractalNoiseMod = id
{-# INLINE fractalNoiseMod #-}
fractalAmpMod :: (Num a) => FractalConfig a -> a -> a
fractalAmpMod FractalConfig{..} n = lerp 1 n weightedStrength
{-# INLINE fractalAmpMod #-}

billowNoiseMod :: (Num a) => a -> a
billowNoiseMod n = abs n * 2 - 1
{-# INLINE billowNoiseMod #-}

billowAmpMod :: (Num a) => FractalConfig a -> a -> a
billowAmpMod FractalConfig{..} n = lerp 1 n weightedStrength
{-# INLINE billowAmpMod #-}

ridgedNoiseMod :: (Num a) => a -> a
ridgedNoiseMod n = abs n * (-2) + 1
{-# INLINE ridgedNoiseMod #-}

ridgedAmpMod :: (Num a) => FractalConfig a -> a -> a
ridgedAmpMod FractalConfig{..} n = lerp 1 (1 - n) weightedStrength
{-# INLINE ridgedAmpMod #-}

newtype PingPongStrength a = PingPongStrength a
  deriving (Generic)

defaultPingPongStrength :: (RealFrac a) => PingPongStrength a
defaultPingPongStrength = PingPongStrength 2
{-# INLINE defaultPingPongStrength #-}

pingPongNoiseMod :: (RealFrac a) => PingPongStrength a -> a -> a
pingPongNoiseMod (PingPongStrength s) n =
  let n' = (n + 1) * s
      t = n' - fromIntegral @Int (truncate (n' * 0.5) * 2)
   in if t < 1 then t else 2 - t
{-# INLINE pingPongNoiseMod #-}

pingPongAmpMod :: (Num a) => FractalConfig a -> a -> a
pingPongAmpMod FractalConfig{..} n = lerp 1 n weightedStrength
{-# INLINE pingPongAmpMod #-}