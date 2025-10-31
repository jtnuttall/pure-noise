{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}

-- |
-- Maintainer: Jeremy Nuttall <jeremy@jeremy-nuttall.com>
-- Stability : experimental
--
-- Performant noise generation with composable noise functions.
--
-- Noise functions are built on a unified 'Noise' type that abstracts over
-- the seed and coordinate parameters. 'Noise2' and 'Noise3' are convenient
-- type aliases for 2D and 3D noise. These can be composed using algebraically
-- with minimal performance overhead.
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
--
-- == Advanced Features
--
-- Generate 1D noise by slicing higher-dimensional noise:
--
-- @
-- noise1d :: Noise.Noise1 Float
-- noise1d = Noise.sliceY2 0.5 Noise.perlin2
--
-- evaluate :: Float -> Float
-- evaluate = Noise.noise1At noise1d 0
-- @
--
-- Transform coordinates with 'warp':
--
-- @
-- scaledAndLayered :: Noise.Noise2 Float
-- scaledAndLayered =
--  Noise.warp (\\(x, y) -> (x * 2, y * 2)) Noise.perlin2
--    + fmap (logBase 2) Noise.perlin2
-- @
--
-- Layer independent noise with 'reseed' or 'next2':
--
-- @
-- layered :: Noise.Noise2 Float
-- layered = Noise.perlin2 + Noise.next2 Noise.perlin2 \/ 2
-- @
module Numeric.Noise (
  -- * Noise

  --

  -- | 'Noise2' and 'Noise3' are type aliases for 2D and 3D noise functions built
  -- on the unified 'Noise' type. They can be evaluated with 'noise2At' and
  -- 'noise3At' respectively.
  --
  -- 'Seed' is a 'Word64' value used for deterministic noise generation.
  Noise,
  Noise1,
  Noise1',
  Noise2,
  Noise2',
  Noise3,
  Noise3',
  Seed,

  -- * Accessors
  noise1At,
  noise2At,
  noise3At,

  -- * Noise functions

  -- ** Perlin
  perlin2,
  perlin3,

  -- ** OpenSimplex
  openSimplex2,

  -- ** OpenSimplex2S
  superSimplex2,

  -- ** Cellular
  cellular2,

  -- *** Configuration
  CellularConfig (..),
  defaultCellularConfig,
  CellularDistanceFn (..),
  CellularResult (..),

  -- ** Value
  value2,
  valueCubic2,
  value3,
  valueCubic3,

  -- ** Constant fields
  const2,
  const3,

  -- * Noise alteration

  --  ** Altering values
  remap,
  --  ** Altering parameters
  warp,
  reseed,
  next2,
  next3,

  -- ** Slicing (projecting)
  sliceX2,
  sliceX3,
  sliceY2,
  sliceY3,
  sliceZ3,

  -- * Fractals

  --

  -- | Fractal noise combines multiple octaves at different frequencies and
  -- amplitudes to create natural-looking, multi-scale patterns.
  --
  -- For custom fractal implementations using modifier functions, see
  -- "Numeric.Noise.Fractal".

  -- ** Fractal Brownian Motion (FBM)
  fractal2,
  fractal3,

  -- ** Fractal variants
  billow2,
  billow3,
  ridged2,
  ridged3,
  pingPong2,
  pingPong3,

  -- ** Configuration
  FractalConfig (..),
  defaultFractalConfig,
  PingPongStrength (..),
  defaultPingPongStrength,

  -- * Math utilities
  clamp,
  clamp2,
  clamp3,
  cubicInterp,
  hermiteInterp,
  lerp,
  quinticInterp,
) where

import Numeric.Noise.Cellular (CellularConfig, CellularDistanceFn (..), CellularResult (..), defaultCellularConfig)
import Numeric.Noise.Cellular qualified as Cellular
import Numeric.Noise.Fractal
import Numeric.Noise.Internal
import Numeric.Noise.OpenSimplex qualified as OpenSimplex
import Numeric.Noise.Perlin qualified as Perlin
import Numeric.Noise.SuperSimplex qualified as SuperSimplex
import Numeric.Noise.Value qualified as Value
import Numeric.Noise.ValueCubic qualified as ValueCubic

-- | 2D Cellular (Worley) noise. Configure with 'CellularConfig' to control
-- distance functions and return values.
--
-- Cellular noise creates patterns based on distances to randomly distributed
-- cell points.
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
