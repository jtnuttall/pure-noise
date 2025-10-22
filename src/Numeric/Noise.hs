{-# LANGUAGE Strict #-}

-- |
-- Maintainer: Jeremy Nuttall <jeremy@jeremy-nuttall.com>
-- Stability : experimental
--
-- Performant noise generation with composable noise functions.
--
-- Noise functions are wrapped in 'Noise2' and 'Noise3' newtypes that abstract over
-- the seed and coordinate parameters. These can be composed using 'Num' or 'Fractional'
-- methods with minimal performance overhead.
--
-- Noise values are generally clamped to @[-1, 1]@, though some functions may
-- occasionally produce values slightly outside this range.
--
-- == Basic Usage
--
-- Generate 2D Perlin noise:
--
-- @
-- import Numeric.Noise qualified as Noise
--
-- myNoise :: Noise.Seed -> Float -> Float -> Float
-- myNoise = Noise.noise2At Noise.perlin2
-- @
--
-- Compose multiple noise functions:
--
-- @
-- combined :: (RealFrac a) => Noise.Noise2 a
-- combined = (Noise.perlin2 + Noise.superSimplex2) / 2
--
-- myNoise2 :: Noise.Seed -> Float -> Float -> Float
-- myNoise2 = Noise.noise2At combined
-- @
--
-- Apply fractal Brownian motion:
--
-- @
-- fbm :: (RealFrac a) => Noise.Noise2 a
-- fbm = Noise.fractal2 Noise.defaultFractalConfig Noise.perlin2
-- @
module Numeric.Noise (
  -- * Core Types

  --

  -- | 'Noise2' and 'Noise3' are newtypes wrapping noise functions. They can be
  -- unwrapped with 'noise2At' and 'noise3At' respectively.
  --
  -- 'Seed' is a 'Word64' value used for deterministic noise generation.
  module NoiseTypes,

  -- * Noise evaluation
  noise2At,
  noise3At,

  -- ** 2D Noise
  const2,
  cellular2,
  openSimplex2,
  superSimplex2,
  perlin2,
  value2,
  valueCubic2,

  -- ** 3D Noise
  const3,
  perlin3,
  value3,
  valueCubic3,

  -- * Noise manipulation

  -- ** Math utility functions
  module NoiseUtility,

  -- ** Fractal noise composition

  --

  -- | Fractal noise combines multiple octaves at different frequencies and
  -- amplitudes to create natural-looking, multi-scale patterns.
  --
  -- For custom fractal implementations using modifier functions, see
  -- "Numeric.Noise.Fractal".

  -- *** Configuration
  FractalConfig (..),
  defaultFractalConfig,
  PingPongStrength (..),
  defaultPingPongStrength,

  -- *** Fractal Brownian Motion (FBM)
  fractal2,
  fractal3,

  -- *** Fractal variants
  billow2,
  billow3,
  ridged2,
  ridged3,
  pingPong2,
  pingPong3,

  -- ** Cellular noise configuration

  --

  -- | Cellular (Worley) noise creates patterns based on distances to
  -- randomly distributed cell points.

  -- *** Configuration
  CellularConfig (..),
  defaultCellularConfig,
  CellularDistanceFn (..),
  CellularResult (..),
) where

import Numeric.Noise.Cellular (CellularConfig, CellularDistanceFn (..), CellularResult (..), defaultCellularConfig)
import Numeric.Noise.Cellular qualified as Cellular
import Numeric.Noise.Fractal
import Numeric.Noise.Internal
import Numeric.Noise.Internal as NoiseTypes (
  Noise2,
  Noise3,
  Seed,
 )
import Numeric.Noise.Internal as NoiseUtility (
  clamp,
  clamp2,
  clamp3,
  cubicInterp,
  hermiteInterp,
  lerp,
  next2,
  next3,
  quinticInterp,
 )
import Numeric.Noise.OpenSimplex qualified as OpenSimplex
import Numeric.Noise.Perlin qualified as Perlin
import Numeric.Noise.SuperSimplex qualified as SuperSimplex
import Numeric.Noise.Value qualified as Value
import Numeric.Noise.ValueCubic qualified as ValueCubic

-- | Evaluate a 2D noise function at the given coordinates with the given seed.
noise2At
  :: Noise2 a
  -> Seed
  -- ^ deterministic seed
  -> a
  -- ^ x coordinate
  -> a
  -- ^ y coordinate
  -> a
noise2At = unNoise2
{-# INLINE noise2At #-}

-- | 2D Cellular (Worley) noise. Configure with 'CellularConfig' to control
-- distance functions and return values.
cellular2 :: (RealFrac a, Floating a) => CellularConfig a -> Noise2 a
cellular2 = Cellular.noise2
{-# INLINE cellular2 #-}

-- | 2D OpenSimplex noise. Smooth gradient noise similar to Perlin but without
-- directional artifacts.
openSimplex2 :: (RealFrac a) => Noise2 a
openSimplex2 = OpenSimplex.noise2
{-# INLINE openSimplex2 #-}

-- | 2D SuperSimplex noise. Improved OpenSimplex variant with better visual
-- characteristics.
superSimplex2 :: (RealFrac a) => Noise2 a
superSimplex2 = SuperSimplex.noise2
{-# INLINE superSimplex2 #-}

-- | 2D Perlin noise. Classic gradient noise algorithm.
perlin2 :: (RealFrac a) => Noise2 a
perlin2 = Perlin.noise2
{-# INLINE perlin2 #-}

-- | Evaluate a 3D noise function at the given coordinates with the given seed.
noise3At
  :: Noise3 a
  -> Seed
  -- ^ deterministic seed
  -> a
  -- ^ x coordinate
  -> a
  -- ^ y coordinate
  -> a
  -- ^ z coordinate
  -> a
noise3At = unNoise3
{-# INLINE noise3At #-}

-- | 3D Perlin noise. Classic gradient noise algorithm.
perlin3 :: (RealFrac a) => Noise3 a
perlin3 = Perlin.noise3
{-# INLINE perlin3 #-}

-- | 2D Value noise. Simple noise based on interpolated random values at grid points.
value2 :: (RealFrac a) => Noise2 a
value2 = Value.noise2
{-# INLINE value2 #-}

-- | 3D Value noise. Simple noise based on interpolated random values at grid points.
value3 :: (RealFrac a) => Noise3 a
value3 = Value.noise3
{-# INLINE value3 #-}

-- | 2D Value noise with cubic interpolation for smoother results.
valueCubic2 :: (RealFrac a) => Noise2 a
valueCubic2 = ValueCubic.noise2
{-# INLINE valueCubic2 #-}

-- | 3D Value noise with cubic interpolation for smoother results.
valueCubic3 :: (RealFrac a) => Noise3 a
valueCubic3 = ValueCubic.noise3
{-# INLINE valueCubic3 #-}
