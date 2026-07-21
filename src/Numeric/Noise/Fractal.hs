{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}

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

  -- * Custom fractals

  --

  -- |
  -- The building blocks 'fractal2', 'billow2', 'ridged2', and 'pingPong2'
  -- are assembled from a shared per-octave loop ('fractal2With' /
  -- 'fractal3With') and a per-variant 'FractalStep'.
  --
  -- You can provide your own step to build custom fractal variants with the
  -- __same specialization behavior as the built-ins__.
  FractalStep,
  fractal2With,
  fractal3With,
  fbmStep,
  billowStep,
  ridgedStep,
  pingPongStep,
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
fractal2 config = mkNoise2 . fractal2With (fbmStep (weightedStrength config)) config . noise2At
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
billow2 config = mkNoise2 . fractal2With (billowStep (weightedStrength config)) config . noise2At
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
ridged2 config = mkNoise2 . fractal2With (ridgedStep (weightedStrength config)) config . noise2At
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
  mkNoise2 . fractal2With (pingPongStep strength (weightedStrength config)) config . noise2At
{-# INLINE [2] pingPong2 #-}

-- | Per-octave fractal step: from the raw octave noise, produce
-- @(# term added to the sum (pre-amplitude), amplitude weight factor #)@.
--
-- The weight factor is multiplied into the amplitude after each octave
-- (before the 'gain' multiply), so it already incorporates
-- 'weightedStrength' — see 'fbmStep' for the canonical shape.
--
-- Consuming this type needs no extensions; /writing/ a custom step requires
-- @{-\# LANGUAGE UnboxedTuples \#-}@:
--
-- @
-- -- fBm with unweighted octaves (weight factor 1)
-- flatStep :: FractalStep Float
-- flatStep raw = (# raw, 1 #)
--
-- custom :: Noise2 Float
-- custom = mkNoise2 (fractal2With flatStep defaultFractalConfig (noise2At perlin2))
-- @
type FractalStep a = a -> (# a, a #)

fractal2With
  :: (RealFrac a)
  => FractalStep a
  -> FractalConfig a
  -> (Seed -> a -> a -> a)
  -> Seed
  -> a
  -> a
  -> a
fractal2With step FractalConfig{..} noise2 seed x0 y0
  | octaves < 1 = 0
  | otherwise =
      let !bounding = fractalBounding FractalConfig{..}
       in go octaves 0 seed x0 y0 bounding
 where
  -- Mirrors FNL's GenFractal* loops, including rounding order:
  -- sum += v * amp; amp *= w; amp *= gain; x *= lacunarity. Carrying the
  -- scaled coordinates (rather than an accumulated freq) matches FNL's
  -- loop and is one multiply cheaper per axis. Caveat: for bases with a
  -- coordinate transform (OpenSimplex2/2S), FNL scales the *transformed*
  -- coordinates while we re-transform the scaled raw ones — identical
  -- rounding only at power-of-two lacunarity, ulps apart otherwise.
  go 0 !acc !_ !_ !_ !_ = acc
  go !o !acc !s !x !y !amp =
    case step (noise2 s x y) of
      (# v, w #) ->
        let !acc' = acc + v * amp
            !amp' = amp * w * gain
         in go (o - 1) acc' (s + 1) (x * lacunarity) (y * lacunarity) amp'
{-# INLINE [1] fractal2With #-}

-- | Apply Fractal Brownian Motion (FBM) to a 3D noise function.
--
-- 3D version of 'fractal2'. See 'fractal2' for details.
fractal3 :: (RealFrac a) => FractalConfig a -> Noise3 a -> Noise3 a
fractal3 config = mkNoise3 . fractal3With (fbmStep (weightedStrength config)) config . noise3At
{-# INLINE [2] fractal3 #-}

-- | Apply billow fractal to a 3D noise function.
--
-- 3D version of 'billow2'. See 'billow2' for details.
billow3 :: (RealFrac a) => FractalConfig a -> Noise3 a -> Noise3 a
billow3 config = mkNoise3 . fractal3With (billowStep (weightedStrength config)) config . noise3At
{-# INLINE [2] billow3 #-}

-- | Apply ridged fractal to a 3D noise function.
--
-- 3D version of 'ridged2'. See 'ridged2' for details.
ridged3 :: (RealFrac a) => FractalConfig a -> Noise3 a -> Noise3 a
ridged3 config = mkNoise3 . fractal3With (ridgedStep (weightedStrength config)) config . noise3At
{-# INLINE [2] ridged3 #-}

-- | Apply ping-pong fractal to a 3D noise function.
--
-- 3D version of 'pingPong2'. See 'pingPong2' for details.
pingPong3 :: (RealFrac a) => FractalConfig a -> PingPongStrength a -> Noise3 a -> Noise3 a
pingPong3 config strength =
  mkNoise3 . fractal3With (pingPongStep strength (weightedStrength config)) config . noise3At
{-# INLINE [2] pingPong3 #-}

fractal3With
  :: (RealFrac a)
  => FractalStep a
  -> FractalConfig a
  -> (Seed -> a -> a -> a -> a)
  -> Seed
  -> a
  -> a
  -> a
  -> a
fractal3With step FractalConfig{..} noise3 seed x0 y0 z0
  | octaves < 1 = 0
  | otherwise =
      let !bounding = fractalBounding FractalConfig{..}
       in go octaves 0 seed x0 y0 z0 bounding
 where
  go 0 !acc !_ !_ !_ !_ !_ = acc
  go !o !acc !s !x !y !z !amp =
    case step (noise3 s x y z) of
      (# v, w #) ->
        let !acc' = acc + v * amp
            !amp' = amp * w * gain
         in go (o - 1) acc' (s + 1) (x * lacunarity) (y * lacunarity) (z * lacunarity) amp'
{-# INLINE [1] fractal3With #-}

fractalBounding :: (RealFrac a) => FractalConfig a -> a
fractalBounding FractalConfig{..} = recip (go 1 g 1)
 where
  -- FNL's CalculateFractalBounding, exactly: gain is abs'd, the sum has
  -- octaves terms (1 + g + ... + g^(octaves-1)), and it accumulates
  -- left-associated: ((1 + g) + g^2) + ... Preserve the shape; sum/take
  -- round differently. octaves = 1 gives 1: a single-octave fractal is
  -- its base noise.
  g = abs gain
  go !i !amp !ampFractal
    | i >= octaves = ampFractal
    | otherwise = go (i + 1) (amp * g) (ampFractal + amp)
{-# INLINE [2] fractalBounding #-}

-- | Step for fractal Brownian motion
--
-- Linear interpolation that maps the octave value from \( [-1, 1] \) into
-- \( [0, 1] \).
--
-- Equivalent to FNL's @GenFractalFBm@, with the exception that it is clamped
-- for both 2D and 3D functions, and so will not exceed \( +1 \).
fbmStep :: (RealFrac a) => a -> FractalStep a
fbmStep wS raw = (# raw, lerp 1 (min (raw + 1) 2 * 0.5) wS #)
{-# INLINE fbmStep #-}

-- | Step for billow fractals
--
-- Creates puffy looking noise. Feels something like clouds.
--
-- See: https://ambient.data-imaginist.com/reference/billow.html
--
-- No direct FastNoiseLite equivalent.
billowStep :: (RealFrac a) => a -> FractalStep a
billowStep wS raw =
  let !b = abs raw * 2 - 1
   in (# b, lerp 1 (min (b + 1) 2 * 0.5) wS #)
{-# INLINE billowStep #-}

-- | Step for ridged fractals
--
-- Creates angular/sharp noise. Feels something like a mountain range.
--
-- Equivalent to FNL's @GenFractalRidged@.
ridgedStep :: (RealFrac a) => a -> FractalStep a
ridgedStep wS raw =
  let !n = abs raw
   in (# n * (-2) + 1, lerp 1 (1 - n) wS #)
{-# INLINE ridgedStep #-}

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

-- | Step for ping-pong fractal,
--
-- Creates wavy, intense noise.
--
-- Equivalent to FNL's @GenFractalPingPong@
pingPongStep :: (RealFrac a) => PingPongStrength a -> a -> FractalStep a
pingPongStep (PingPongStrength strength) wS raw =
  let !n = pingPong ((raw + 1) * strength)
   in (# (n - 0.5) * 2, lerp 1 n wS #)
{-# INLINE pingPongStep #-}

pingPong :: (RealFrac a) => a -> a
pingPong t0 =
  let !t = t0 - fromIntegral @Int (truncate (t0 * 0.5) * 2)
   in if t < 1 then t else 2 - t
{-# INLINE pingPong #-}
