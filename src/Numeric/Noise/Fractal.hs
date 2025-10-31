{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

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

-- | Configuration for fractal noise generation.
--
-- Fractal noise combines multiple octaves (layers) of noise at different
-- frequencies and amplitudes to create more complex, natural-looking patterns.
data FractalConfig a = FractalConfig
  { octaves :: Int
  -- ^ Number of noise layers to combine. More octaves create more detail
  -- but are more expensive to compute. Must be \( >= 1 \).
  , lacunarity :: a
  -- ^ Frequency multiplier between octaves. Each octave's frequency is
  -- the previous octave's frequency multiplied by lacunarity.
  , gain :: a
  -- ^ Amplitude multiplier between octaves. Each octave's amplitude is
  -- the previous octave's amplitude multiplied by gain.
  -- Values \( < 1 \) create smoother noise, values \( > 1 \) create rougher noise.
  , weightedStrength :: a
  -- ^ Controls how much each octave's amplitude is influenced by the
  -- previous octave's value. At 0, octaves have independent amplitudes.
  -- At 1, lower-valued areas in previous octaves reduce the amplitude
  -- of subsequent octaves. Range: \( [0, 1] \).
  }
  deriving (Generic, Read, Show, Eq)

-- | Default configuration for fractal noise generation.
defaultFractalConfig :: (RealFrac a) => FractalConfig a
defaultFractalConfig =
  FractalConfig
    { octaves = 7
    , lacunarity = 2
    , gain = 0.5
    , weightedStrength = 0
    }
{-# INLINEABLE defaultFractalConfig #-}

-- | Apply Fractal Brownian Motion (FBM) to a 2D noise function.
--
-- FBM combines multiple octaves of noise at increasing frequencies and
-- decreasing amplitudes to create natural-looking, multi-scale patterns.
-- This is the standard fractal noise implementation.
--
-- @
-- fbm :: Noise2 Float
-- fbm = fractal2 defaultFractalConfig perlin2
-- @
fractal2 :: (RealFrac a) => FractalConfig a -> Noise2 a -> Noise2 a
fractal2 config = mkNoise2 . fractal2With fractalNoiseMod (fractalAmpMod config) config . noise2At
{-# INLINE [2] fractal2 #-}

-- | Apply billow fractal to a 2D noise function.
--
-- Billow creates a cloud-like or billowy appearance by taking the absolute
-- value of each octave. This produces sharp ridges in the negative regions
-- of the noise, creating a distinct puffy or cloudy look.
--
-- @
-- clouds :: Noise2 Float
-- clouds = billow2 defaultFractalConfig perlin2
-- @
billow2 :: (RealFrac a) => FractalConfig a -> Noise2 a -> Noise2 a
billow2 config = mkNoise2 . fractal2With billowNoiseMod (billowAmpMod config) config . noise2At
{-# INLINE [2] billow2 #-}

-- | Apply ridged fractal to a 2D noise function.
--
-- Ridged creates sharp ridges by inverting and taking the absolute value
-- of each octave. This is particularly useful for terrain generation,
-- creating mountain ridges and valleys.
--
-- @
-- mountains :: Noise2 Float
-- mountains = ridged2 defaultFractalConfig perlin2
-- @
ridged2 :: (RealFrac a) => FractalConfig a -> Noise2 a -> Noise2 a
ridged2 config = mkNoise2 . fractal2With ridgedNoiseMod (ridgedAmpMod config) config . noise2At
{-# INLINE [2] ridged2 #-}

-- | Apply ping-pong fractal to a 2D noise function.
--
-- Ping-pong creates a wave-like pattern by folding the noise values back
-- and forth within a range, creating a distinctive undulating appearance.
-- The strength parameter controls the intensity of the ping-pong effect.
--
-- @
-- waves :: Noise2 Float
-- waves = pingPong2 defaultFractalConfig defaultPingPongStrength perlin2
-- @
pingPong2 :: (RealFrac a) => FractalConfig a -> PingPongStrength a -> Noise2 a -> Noise2 a
pingPong2 config strength =
  mkNoise2 . fractal2With (pingPongNoiseMod strength) (pingPongAmpMod config) config . noise2At
{-# INLINE [2] pingPong2 #-}

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
  | octaves < 1 = 0
  | otherwise =
      let !bounding = fractalBounding FractalConfig{..}
       in go octaves 0 seed 1 bounding
 where
  go 0 !acc !_ !_ !_ = acc
  go !o !acc !s !freq !amp =
    let !noise = amp * modNoise (noise2 s (freq * x) (freq * y))
        !amp' = amp * gain * modAmps (min (noise + 1) 2)
     in go (o - 1) (acc + noise) (s + 1) (freq * lacunarity) amp'
{-# INLINE [1] fractal2With #-}

-- | Apply Fractal Brownian Motion (FBM) to a 3D noise function.
--
-- 3D version of 'fractal2'. See 'fractal2' for details.
fractal3 :: (RealFrac a) => FractalConfig a -> Noise3 a -> Noise3 a
fractal3 config = mkNoise3 . fractal3With fractalNoiseMod (fractalAmpMod config) config . noise3At
{-# INLINE [2] fractal3 #-}

-- | Apply billow fractal to a 3D noise function.
--
-- 3D version of 'billow2'. See 'billow2' for details.
billow3 :: (RealFrac a) => FractalConfig a -> Noise3 a -> Noise3 a
billow3 config = mkNoise3 . fractal3With billowNoiseMod (billowAmpMod config) config . noise3At
{-# INLINE [2] billow3 #-}

-- | Apply ridged fractal to a 3D noise function.
--
-- 3D version of 'ridged2'. See 'ridged2' for details.
ridged3 :: (RealFrac a) => FractalConfig a -> Noise3 a -> Noise3 a
ridged3 config = mkNoise3 . fractal3With ridgedNoiseMod (ridgedAmpMod config) config . noise3At
{-# INLINE [2] ridged3 #-}

-- | Apply ping-pong fractal to a 3D noise function.
--
-- 3D version of 'pingPong2'. See 'pingPong2' for details.
pingPong3 :: (RealFrac a) => FractalConfig a -> PingPongStrength a -> Noise3 a -> Noise3 a
pingPong3 config strength =
  mkNoise3 . fractal3With (pingPongNoiseMod strength) (pingPongAmpMod config) config . noise3At
{-# INLINE [2] pingPong3 #-}

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
  | octaves < 1 = 0
  | otherwise =
      let !bounding = fractalBounding FractalConfig{..}
       in go octaves 0 seed 1 bounding
 where
  go 0 !acc !_ !_ !_ = acc
  go !o !acc !s !freq !amp =
    let !noise = amp * modNoise (noise3 s (freq * x) (freq * y) (freq * z))
        !amp' = amp * gain * modAmps (min (noise + 1) 2)
     in go (o - 1) (acc + noise) (s + 1) (freq * lacunarity) amp'
{-# INLINE [1] fractal3With #-}

fractalBounding :: (RealFrac a) => FractalConfig a -> a
fractalBounding FractalConfig{..} = recip (sum amps + 1)
 where
  amps = take octaves $ iterate (* gain) gain
{-# INLINE [2] fractalBounding #-}

-- | Identity noise modifier for standard FBM.
--
-- This is used internally by 'fractal2' and 'fractal3'.
-- Exposed for users creating custom fractal implementations.
fractalNoiseMod :: a -> a
fractalNoiseMod = id
{-# INLINE fractalNoiseMod #-}

-- | Amplitude modifier for standard FBM.
--
-- Uses the 'weightedStrength' parameter to influence amplitude based on
-- the previous octave's value. Exposed for custom fractal implementations.
fractalAmpMod :: (Num a) => FractalConfig a -> a -> a
fractalAmpMod FractalConfig{..} n = lerp 1 n weightedStrength
{-# INLINE fractalAmpMod #-}

-- | Noise modifier for billow fractal.
--
-- Transforms noise value to @abs(n) * 2 - 1@, creating the billow effect.
-- Exposed for custom fractal implementations.
billowNoiseMod :: (Num a) => a -> a
billowNoiseMod n = abs n * 2 - 1
{-# INLINE billowNoiseMod #-}

-- | Amplitude modifier for billow fractal.
--
-- Uses the 'weightedStrength' parameter. Exposed for custom fractal implementations.
billowAmpMod :: (Num a) => FractalConfig a -> a -> a
billowAmpMod FractalConfig{..} n = lerp 1 n weightedStrength
{-# INLINE billowAmpMod #-}

-- | Noise modifier for ridged fractal.
--
-- Transforms noise value to @abs(n) * (-2) + 1@, creating the ridge effect.
-- Exposed for custom fractal implementations.
ridgedNoiseMod :: (Num a) => a -> a
ridgedNoiseMod n = abs n * (-2) + 1
{-# INLINE ridgedNoiseMod #-}

-- | Amplitude modifier for ridged fractal.
--
-- Uses the 'weightedStrength' parameter with inverted noise value.
-- Exposed for custom fractal implementations.
ridgedAmpMod :: (Num a) => FractalConfig a -> a -> a
ridgedAmpMod FractalConfig{..} n = lerp 1 (1 - n) weightedStrength
{-# INLINE ridgedAmpMod #-}

-- | Strength parameter for ping-pong fractal noise.
--
-- Controls the intensity of the ping-pong folding effect.
-- Higher values create more frequent oscillations.
newtype PingPongStrength a = PingPongStrength a
  deriving (Generic)

-- | Default ping-pong strength value.
defaultPingPongStrength :: (RealFrac a) => PingPongStrength a
defaultPingPongStrength = PingPongStrength 2
{-# INLINE defaultPingPongStrength #-}

-- | Noise modifier for ping-pong fractal.
--
-- Folds noise values back and forth within a range, creating a wave-like
-- pattern. The strength parameter controls the folding intensity.
-- Exposed for custom fractal implementations.
pingPongNoiseMod :: (RealFrac a) => PingPongStrength a -> a -> a
pingPongNoiseMod (PingPongStrength s) n =
  let n' = (n + 1) * s
      t = n' - fromIntegral @Int (truncate (n' * 0.5) * 2)
   in 1 - abs (t - 1)
{-# INLINE pingPongNoiseMod #-}

-- | Amplitude modifier for ping-pong fractal.
--
-- Uses the 'weightedStrength' parameter. Exposed for custom fractal implementations.
pingPongAmpMod :: (Num a) => FractalConfig a -> a -> a
pingPongAmpMod FractalConfig{..} n = lerp 1 n weightedStrength
{-# INLINE pingPongAmpMod #-}
