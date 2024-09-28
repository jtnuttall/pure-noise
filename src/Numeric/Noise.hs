{-# LANGUAGE Strict #-}

-- |
-- Maintainer: Jeremy Nuttall <jeremy@jeremy-nuttall.com>
-- Stability : experimental
module Numeric.Noise (
  -- * Noise functions

  -- ** Noise functions
  module NoiseTypes,
  noise2At,
  noise3At,

  -- ** 2D Noise
  cellular2,
  openSimplex2,
  superSimplex2,
  perlin2,
  value2,
  valueCubic2,

  -- ** 3D Noise
  perlin3,
  value3,
  valueCubic3,

  -- * Noise manipulation

  -- ** Math utility functions
  module NoiseUtility,

  -- ** Fractal Brownian Motion
  module Fractal,

  -- ** Cellular noise configuration
  module Cellular,
) where

import Numeric.Noise.Cellular as Cellular (
  CellularConfig (..),
  CellularDistanceFn (..),
  CellularResult (..),
  defaultCellularConfig,
 )
import Numeric.Noise.Cellular qualified as Cellular
import Numeric.Noise.Fractal as Fractal
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
  quinticInterp,
 )
import Numeric.Noise.OpenSimplex qualified as OpenSimplex
import Numeric.Noise.Perlin qualified as Perlin
import Numeric.Noise.SuperSimplex qualified as SuperSimplex
import Numeric.Noise.Value qualified as Value
import Numeric.Noise.ValueCubic qualified as ValueCubic

noise2At :: Noise2 a -> Seed -> a -> a -> a
noise2At = unNoise2
{-# INLINE noise2At #-}

cellular2 :: (RealFrac a, Floating a) => CellularConfig a -> Noise2 a
cellular2 = Cellular.noise2
{-# INLINE cellular2 #-}

openSimplex2 :: (RealFrac a) => Noise2 a
openSimplex2 = OpenSimplex.noise2
{-# INLINE openSimplex2 #-}

superSimplex2 :: (RealFrac a) => Noise2 a
superSimplex2 = SuperSimplex.noise2
{-# INLINE superSimplex2 #-}

perlin2 :: (RealFrac a) => Noise2 a
perlin2 = Perlin.noise2
{-# INLINE perlin2 #-}

noise3At :: Noise3 a -> Seed -> a -> a -> a -> a
noise3At = unNoise3
{-# INLINE noise3At #-}

perlin3 :: (RealFrac a) => Noise3 a
perlin3 = Perlin.noise3
{-# INLINE perlin3 #-}

value2 :: (RealFrac a) => Noise2 a
value2 = Value.noise2
{-# INLINE value2 #-}

value3 :: (RealFrac a) => Noise3 a
value3 = Value.noise3
{-# INLINE value3 #-}

valueCubic2 :: (RealFrac a) => Noise2 a
valueCubic2 = ValueCubic.noise2
{-# INLINE valueCubic2 #-}

valueCubic3 :: (RealFrac a) => Noise3 a
valueCubic3 = ValueCubic.noise3
{-# INLINE valueCubic3 #-}