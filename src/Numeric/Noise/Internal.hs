{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Maintainer: Jeremy Nuttall <jeremy@jeremy-nuttall.com>
-- Stability : experimental
module Numeric.Noise.Internal (
  module Math,
  NoiseN (..),
  Noise (..),
  SliceNoise (..),
  sliceX,
  sliceY,
  sliceZ,
  Noise2,
  next2,
  map2,
  clamp2,
  const2,
  Noise3,
  next3,
  map3,
  clamp3,
  const3,
) where

import Data.Type.Ord (type (<))
import GHC.TypeLits
import Numeric.Noise.Internal.Math as Math (
  Hash,
  Seed,
  clamp,
  cubicInterp,
  hermiteInterp,
  lerp,
  quinticInterp,
 )

class NoiseN (dim :: Nat) a where
  data Noise dim a

  -- | A noise function that produces the same value everywhere.
  --
  -- Used to provide the 'Num' instance.
  constNoise :: a -> Noise dim a

  -- | Alter the seed for a noise function.
  --
  -- This is useful for generating independent noise layers:
  --
  -- @
  -- layer1 = perlin2
  -- layer2 = mapSeed (+12) perlin2
  -- layer3 = mapSeed (subtract 2) perlin2
  -- @
  mapSeed :: (Seed -> Seed) -> Noise dim a -> Noise dim a

  -- | Map an arbitrary function across a noise field
  mapNoise :: (a -> a) -> Noise dim a -> Noise dim a

  -- | Combine two noise functions pointwise (i.e., by altering their output)
  combineNoise :: (a -> a -> a) -> Noise dim a -> Noise dim a -> Noise dim a

-- | Clamp a noise function between the given lower and higer bound
clampNoise :: (NoiseN dim a, Ord a) => a -> a -> Noise dim a -> Noise dim a
clampNoise l u = mapNoise (clamp l u)
{-# INLINE clampNoise #-}

instance (Num a, NoiseN dim a) => Num (Noise dim a) where
  (+) = combineNoise (+)
  (*) = combineNoise (*)
  abs = mapNoise abs
  signum = mapNoise signum
  fromInteger i = constNoise (fromInteger i)
  negate = mapNoise negate
  {-# INLINE (+) #-}
  {-# INLINE (*) #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  {-# INLINE fromInteger #-}
  {-# INLINE negate #-}

instance (Fractional a, NoiseN dim a) => Fractional (Noise dim a) where
  fromRational r = constNoise (fromRational r)
  recip = mapNoise recip
  (/) = combineNoise (/)
  {-# INLINE fromRational #-}
  {-# INLINE recip #-}
  {-# INLINE (/) #-}

instance (Floating a, NoiseN dim a) => Floating (Noise dim a) where
  pi = constNoise pi
  exp = mapNoise exp
  log = mapNoise log
  sin = mapNoise sin
  cos = mapNoise cos
  asin = mapNoise asin
  acos = mapNoise acos
  atan = mapNoise atan
  sinh = mapNoise sinh
  cosh = mapNoise cosh
  asinh = mapNoise asinh
  acosh = mapNoise acosh
  atanh = mapNoise atanh
  {-# INLINE pi #-}
  {-# INLINE exp #-}
  {-# INLINE log #-}
  {-# INLINE sin #-}
  {-# INLINE cos #-}
  {-# INLINE asin #-}
  {-# INLINE acos #-}
  {-# INLINE atan #-}
  {-# INLINE sinh #-}
  {-# INLINE cosh #-}
  {-# INLINE asinh #-}
  {-# INLINE acosh #-}
  {-# INLINE atanh #-}

class (NoiseN dim a, KnownNat axis) => SliceNoise dim axis a where
  sliceNoise :: (axis < dim) => proxy axis -> a -> Noise dim a -> Noise (dim - 1) a

type AxisX = 0
type AxisY = 1
type AxisZ = 2

sliceX :: (SliceNoise dim AxisX a, AxisX < dim) => a -> Noise dim a -> Noise (dim - 1) a
sliceX = sliceNoise (SNat @AxisX)
{-# INLINE sliceX #-}

sliceY :: (SliceNoise dim AxisY a, AxisY < dim) => a -> Noise dim a -> Noise (dim - 1) a
sliceY = sliceNoise (SNat @AxisY)
{-# INLINE sliceY #-}

sliceZ :: (SliceNoise dim AxisZ a, AxisZ < dim) => a -> Noise dim a -> Noise (dim - 1) a
sliceZ = sliceNoise (SNat @AxisZ)
{-# INLINE sliceZ #-}

instance NoiseN 1 a where
  newtype Noise 1 a = Noise1 {unNoise1 :: Seed -> a -> a}
  constNoise a = Noise1 (\_ _ -> a)
  mapSeed f (Noise1 g) = Noise1 (g . f)
  mapNoise f (Noise1 g) = Noise1 (\s x -> f (g s x))
  combineNoise f (Noise1 g) (Noise1 h) = Noise1 (\s x -> g s x `f` h s x)
  {-# INLINE constNoise #-}
  {-# INLINE mapSeed #-}
  {-# INLINE mapNoise #-}
  {-# INLINE combineNoise #-}

-- | A 2D noise function parameterized by a seed and two coordinates.
--
-- 'Noise2' wraps a function @Seed -> a -> a -> a@ that takes a seed value
-- and x, y coordinates to produce a noise value.
--
-- This type supports 'Num', 'Fractional', and 'Floating' instances, allowing
-- noise functions to be combined algebraically:
--
-- @
-- combined :: Noise2 Float
-- combined = (perlin2 + superSimplex2) / 2
-- @
--
-- To evaluate a 'Noise2', use 'noise2At' from "Numeric.Noise".
instance NoiseN 2 a where
  newtype Noise 2 a = Noise2 {unNoise2 :: Seed -> a -> a -> a}
  constNoise a = Noise2 (\_ _ _ -> a)
  mapSeed f (Noise2 g) = Noise2 (g . f)
  mapNoise f (Noise2 g) = Noise2 (\s x y -> f (g s x y))
  combineNoise f (Noise2 g) (Noise2 h) = Noise2 (\s x y -> g s x y `f` h s x y)
  {-# INLINE constNoise #-}
  {-# INLINE mapSeed #-}
  {-# INLINE mapNoise #-}
  {-# INLINE combineNoise #-}

-- | Convenience wrapper for $Noise 2 a$
type Noise2 a = Noise 2 a

instance SliceNoise 2 AxisX a where
  sliceNoise _ a (Noise2 f) = Noise1 (\s x -> f s a x)
  {-# INLINE sliceNoise #-}

instance SliceNoise 2 AxisY a where
  sliceNoise _ a (Noise2 f) = Noise1 (\s x -> f s x a)
  {-# INLINE sliceNoise #-}

-- | Increment the seed for a 2D noise function.
--
-- Equivalent to $mapSeed (+1)$
next2 :: Noise 2 a -> Noise 2 a
next2 = mapSeed (+ 1)
{-# INLINE next2 #-}

-- | Map an arbitrary function across a noise field
map2 :: (a -> a) -> Noise 2 a -> Noise 2 a
map2 = mapNoise
{-# INLINE map2 #-}

-- | Clamp the output of a 2D noise function to the range @[lower, upper]@.
--
-- @
-- clamped :: Noise2 Float
-- clamped = clamp2 0 1 perlin2  -- clamp to [0, 1]
-- @
clamp2 :: (Ord a) => a -> a -> Noise 2 a -> Noise 2 a
clamp2 = clampNoise
{-# INLINE clamp2 #-}

-- | A noise function that produces the same value everywhere.
--
-- Used to provide the 'Num' instance.
const2 :: a -> Noise 2 a
const2 = constNoise
{-# INLINE const2 #-}

-- | A 3D noise function parameterized by a seed and three coordinates.
--
-- 'Noise3' wraps a function @Seed -> a -> a -> a -> a@ that takes a seed value
-- and x, y, z coordinates to produce a noise value.
--
-- Like 'Noise2', this type supports 'Num', 'Fractional', and 'Floating' instances
-- for algebraic composition.
--
-- To evaluate a 'Noise3', use 'noise3At' from "Numeric.Noise".
instance NoiseN 3 a where
  newtype Noise 3 a = Noise3 {unNoise3 :: Seed -> a -> a -> a -> a}
  constNoise a = Noise3 (\_ _ _ _ -> a)
  mapSeed f (Noise3 g) = Noise3 (g . f)
  mapNoise f (Noise3 g) = Noise3 (\s x y z -> f (g s x y z))
  combineNoise f (Noise3 g) (Noise3 h) = Noise3 (\s x y z -> g s x y z `f` h s x y z)
  {-# INLINE constNoise #-}
  {-# INLINE mapSeed #-}
  {-# INLINE mapNoise #-}
  {-# INLINE combineNoise #-}

-- | Convenience wrapper for $Noise 3 a$
type Noise3 a = Noise 3 a

instance SliceNoise 3 AxisX a where
  sliceNoise _ a (Noise3 f) = Noise2 (\s x y -> f s a x y)
  {-# INLINE sliceNoise #-}

instance SliceNoise 3 AxisY a where
  sliceNoise _ a (Noise3 f) = Noise2 (\s x y -> f s x a y)
  {-# INLINE sliceNoise #-}

instance SliceNoise 3 AxisZ a where
  sliceNoise _ a (Noise3 f) = Noise2 (\s x y -> f s x y a)
  {-# INLINE sliceNoise #-}

-- | Increment the seed for a 3D noise function.
--
-- Analogous to 'next2', this is useful for generating independent 3D noise layers.
next3 :: Noise 3 a -> Noise 3 a
next3 = mapSeed (+ 1)
{-# INLINE next3 #-}

-- | Map an arbitrary function across a noise field
map3 :: (a -> a) -> Noise 3 a -> Noise 3 a
map3 = mapNoise
{-# INLINE map3 #-}

-- | A noise function that produces the same value everywhere.
--
-- Used to provide the 'Num' instance.
const3 :: a -> Noise 3 a
const3 = constNoise
{-# INLINE const3 #-}

-- | Clamp the output of a 3D noise function to the range @[lower, upper]@.
--
-- @
-- clamped :: Noise3 Float
-- clamped = clamp3 (-0.5) 0.5 perlin3  -- clamp to [-0.5, 0.5]
-- @
clamp3 :: (Ord a) => a -> a -> Noise 3 a -> Noise 3 a
clamp3 = clampNoise
{-# INLINE clamp3 #-}
