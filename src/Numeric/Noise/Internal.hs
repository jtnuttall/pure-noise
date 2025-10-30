-- |
-- Maintainer: Jeremy Nuttall <jeremy@jeremy-nuttall.com>
-- Stability : experimental
module Numeric.Noise.Internal (
  module Math,
  Noise (..),
  constant,
  remap,
  warp,
  reseed,
  blend,
  sliceX2,
  sliceY2,
  sliceX3,
  sliceY3,
  sliceZ3,
  Noise1,
  mkNoise1,
  unNoise1,
  Noise2,
  mkNoise2,
  unNoise2,
  next2,
  map2,
  clamp2,
  const2,
  Noise3,
  mkNoise3,
  unNoise3,
  next3,
  map3,
  clamp3,
  const3,
) where

import Numeric.Noise.Internal.Math as Math (
  Hash,
  Seed,
  clamp,
  cubicInterp,
  hermiteInterp,
  lerp,
  quinticInterp,
 )

-- |  'Noise' represents a function from a 'Seed' and coordinates @p@ to a noise
-- value @v@.
--
-- For convenience, dimension-specific type aliases are provided:
--
-- === Instances
--
-- 'Noise' can be composed algebraically:
--
-- @
-- combined :: Noise (Float, Float) Float
-- combined = (perlin2 + superSimplex2) / 2
-- @
--
-- === Coordinate Transformation
--
-- Noise is contravariant in the coordinate parameter and covariant in the
-- value parameter. Use 'warp' to transform coordinates and 'remap' (or
-- 'fmap') to transform values.
--
-- @
-- scaled :: Noise2 Float
-- scaled = warp (\\(x, y) -> (x * 2, y * 2)) perlin2
-- @
--
-- === Evaluation
--
-- To evaluate noise functions, use 'noise1At', 'noise2At', or 'noise3At' from
-- "Numeric.Noise".
newtype Noise p v = Noise {unNoise :: Seed -> p -> v}

type Noise1' p v = Noise p v
type Noise1 v = Noise1' v v

mkNoise1 :: (Seed -> p -> v) -> Noise1' p v
mkNoise1 = Noise
{-# INLINE mkNoise1 #-}

unNoise1 :: Noise p v -> Seed -> p -> v
unNoise1 = unNoise
{-# INLINE unNoise1 #-}

type Noise2' p v = Noise (p, p) v
type Noise2 v = Noise2' v v

mkNoise2 :: (Seed -> p -> p -> v) -> Noise2' p v
mkNoise2 f = Noise (\s (x, y) -> f s x y)
{-# INLINE mkNoise2 #-}

unNoise2 :: Noise2' p v -> Seed -> p -> p -> v
unNoise2 (Noise f) seed x y = f seed (x, y)
{-# INLINE unNoise2 #-}

type Noise3' p v = Noise (p, p, p) v
type Noise3 v = Noise3' v v

mkNoise3 :: (Seed -> p -> p -> p -> v) -> Noise3' p v
mkNoise3 f = Noise (\s (x, y, z) -> f s x y z)
{-# INLINE mkNoise3 #-}

unNoise3 :: Noise3' p v -> Seed -> p -> p -> p -> v
unNoise3 (Noise f) seed x y z = f seed (x, y, z)
{-# INLINE unNoise3 #-}

instance Functor (Noise c) where
  fmap f (Noise g) = Noise (\seed -> f . g seed)
  {-# INLINE fmap #-}

instance Applicative (Noise p) where
  pure a = Noise $ \_ _ -> a
  liftA2 f (Noise g) (Noise h) = Noise (\s p -> g s p `f` h s p)
  {-# INLINE pure #-}
  {-# INLINE liftA2 #-}

instance Monad (Noise p) where
  Noise g >>= f = Noise (\s p -> unNoise (f (g s p)) s p)
  {-# INLINE (>>=) #-}

instance (Num a) => Num (Noise p a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger i = pure (fromInteger i)
  negate = fmap negate
  {-# INLINE (+) #-}
  {-# INLINE (*) #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  {-# INLINE fromInteger #-}
  {-# INLINE negate #-}

instance (Fractional a) => Fractional (Noise p a) where
  fromRational = pure . fromRational
  recip = fmap recip
  (/) = liftA2 (/)
  {-# INLINE fromRational #-}
  {-# INLINE recip #-}
  {-# INLINE (/) #-}

instance (Floating a) => Floating (Noise p a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh
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

-- | Transform the values produced by a noise function.
--
-- This is an alias for 'fmap'. Use it to transform noise values after generation:
--
-- @
-- -- Scale noise from [-1, 1] to [0, 1]
-- normalized :: Noise2 Float
-- normalized = remap (\\x -> (x + 1) / 2) perlin2
-- @
remap :: (a -> b) -> Noise p a -> Noise p b
remap = fmap
{-# INLINE remap #-}

-- | Transform the coordinate space of a noise function.
--
-- This allows you to scale, rotate, or otherwise modify coordinates before
-- they're passed to the noise function:
--
-- @
-- -- Scale the noise frequency
-- scaled :: Noise2 Float
-- scaled = warp (\\(x, y) -> (x * 2, y * 2)) perlin2
--
-- -- Rotate the noise field
-- rotated :: Noise2 Float
-- rotated = warp (\\(x, y) -> (x * cos a - y * sin a, x * sin a + y * cos a)) perlin2
--   where a = pi / 4
-- @
--
-- NB: This is 'contramap'
warp :: (p -> p') -> Noise p' v -> Noise p v
warp f (Noise g) = Noise (\s p -> g s (f p))
{-# INLINE warp #-}

-- | Modify the seed used by a noise function.
--
-- This is useful for generating independent layers of noise:
--
-- @
-- layer1 = perlin2
-- layer2 = reseed (+1) perlin2
-- layer3 = reseed (+2) perlin2
--
-- combined = (layer1 + layer2 + layer3) / 3
-- @
--
-- See also 'next2' and 'next3' for convenient increment-by-one variants.
reseed :: (Seed -> Seed) -> Noise p a -> Noise p a
reseed f (Noise g) = Noise (g . f)
{-# INLINE reseed #-}

constant :: a -> Noise c a
constant = pure
{-# INLINE constant #-}

-- | Combine two noise functions with a custom blending function.
--
-- This is an alias for 'liftA2'. Use it to mix multiple noise sources:
--
-- @
-- -- Multiply two noise functions
-- multiplied :: Noise2 Float
-- multiplied = blend (*) perlin2 superSimplex2
--
-- -- Custom blending based on values
-- custom :: Noise2 Float
-- custom = blend (\\a b -> if a > 0 then a else b) perlin2 superSimplex2
-- @
blend :: (a -> b -> c) -> Noise p a -> Noise p b -> Noise p c
blend = liftA2
{-# INLINE blend #-}

-- | Clamp a noise function between the given lower and higher bound
clampNoise :: (Ord a) => a -> a -> Noise p a -> Noise p a
clampNoise l u = fmap (clamp l u)
{-# INLINE clampNoise #-}

-- | Slice a 2D noise function at a fixed X coordinate to produce 1D noise.
--
-- @
-- noise1d :: Noise1 Float
-- noise1d = sliceX2 0.0 perlin2  -- Fix X at 0, vary Y
--
-- -- Evaluate at Y = 5.0
-- value = noise1At noise1d seed 5.0
-- @
sliceX2 :: p -> Noise2' p v -> Noise1' p v
sliceX2 x = warp (x,)
{-# INLINE sliceX2 #-}

-- | Slice a 2D noise function at a fixed Y coordinate to produce 1D noise.
--
-- @
-- noise1d :: Noise1 Float
-- noise1d = sliceY2 0.0 perlin2  -- Fix Y at 0, vary X
--
-- -- Evaluate at X = 5.0
-- value = noise1At noise1d seed 5.0
-- @
sliceY2 :: p -> Noise2' p v -> Noise1' p v
sliceY2 y = warp (,y)
{-# INLINE sliceY2 #-}

-- | Slice a 3D noise function at a fixed X coordinate to produce 2D noise.
--
-- @
-- noise2d :: Noise2 Float
-- noise2d = sliceX3 0.0 perlin3  -- Fix X at 0, vary Y and Z
--
-- -- Evaluate at Y = 1.0, Z = 2.0
-- value = noise2At noise2d seed 1.0 2.0
-- @
sliceX3 :: p -> Noise3' p v -> Noise2' p v
sliceX3 x = warp (\(y, z) -> (x, y, z))
{-# INLINE sliceX3 #-}

-- | Slice a 3D noise function at a fixed Y coordinate to produce 2D noise.
--
-- @
-- noise2d :: Noise2 Float
-- noise2d = sliceY3 0.0 perlin3  -- Fix Y at 0, vary X and Z
-- @
sliceY3 :: p -> Noise3' p v -> Noise2' p v
sliceY3 y = warp (\(x, z) -> (x, y, z))
{-# INLINE sliceY3 #-}

-- | Slice a 3D noise function at a fixed Z coordinate to produce 2D noise.
--
-- This is useful for extracting 2D slices from 3D noise at different heights:
--
-- @
-- heightmap :: Noise2 Float
-- heightmap = sliceZ3 10.0 perlin3  -- Sample at Z = 10
-- @
sliceZ3 :: p -> Noise3' p v -> Noise2' p v
sliceZ3 z = warp (\(x, y) -> (x, y, z))
{-# INLINE sliceZ3 #-}

-- | Increment the seed for a 2D noise function.
--
-- Equivalent to $reseed (+1)$
next2 :: Noise2 a -> Noise2 a
next2 = reseed (+ 1)
{-# INLINE next2 #-}

-- | Map an arbitrary function across a noise field
map2 :: (a -> a) -> Noise2 a -> Noise2 a
map2 = fmap
{-# INLINE map2 #-}

-- | Clamp the output of a 2D noise function to the range @[lower, upper]@.
--
-- @
-- clamped :: Noise2 Float
-- clamped = clamp2 0 1 perlin2  -- clamp to [0, 1]
-- @
clamp2 :: (Ord a) => a -> a -> Noise2 a -> Noise2 a
clamp2 = clampNoise
{-# INLINE clamp2 #-}

-- | A noise function that produces the same value everywhere.
--
-- Used to provide the 'Num' instance.
const2 :: a -> Noise2 a
const2 = pure
{-# INLINE const2 #-}

-- | Increment the seed for a 3D noise function.
--
-- Analogous to 'next2', this is useful for generating independent 3D noise layers.
next3 :: Noise3 a -> Noise3 a
next3 = reseed (+ 1)
{-# INLINE next3 #-}

-- | Map an arbitrary function across a noise field
map3 :: (a -> a) -> Noise3 a -> Noise3 a
map3 = fmap
{-# INLINE map3 #-}

-- | A noise function that produces the same value everywhere.
--
-- Used to provide the 'Num' instance.
const3 :: a -> Noise3 a
const3 = pure
{-# INLINE const3 #-}

-- | Clamp the output of a 3D noise function to the range @[lower, upper]@.
--
-- @
-- clamped :: Noise3 Float
-- clamped = clamp3 (-0.5) 0.5 perlin3  -- clamp to [-0.5, 0.5]
-- @
clamp3 :: (Ord a) => a -> a -> Noise3 a -> Noise3 a
clamp3 = clampNoise
{-# INLINE clamp3 #-}
