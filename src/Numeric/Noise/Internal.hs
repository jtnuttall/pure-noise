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

import Data.Coerce
import Data.Functor.Contravariant
import Numeric.Noise.Internal.Math as Math (
  Hash,
  Seed,
  clamp,
  cubicInterp,
  hermiteInterp,
  lerp,
  quinticInterp,
 )

-- | TODO: docs
--
-- Technically this admits a Profunctor instance, but I don't want to depend
-- on profunctors.
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

remap :: (a -> b) -> Noise p a -> Noise p b
remap = fmap
{-# INLINE remap #-}

-- | 'contramap'
warp :: (p -> p') -> Noise p' v -> Noise p v
warp f (Noise g) = coerce (\s p -> g s (f p))
{-# INLINE warp #-}

reseed :: (Seed -> Seed) -> Noise p a -> Noise p a
reseed f (Noise g) = Noise (g . f)
{-# INLINE reseed #-}

constant :: a -> Noise c a
constant = pure
{-# INLINE constant #-}

blend :: (a -> b -> c) -> Noise p a -> Noise p b -> Noise p c
blend = liftA2
{-# INLINE blend #-}

-- | Clamp a noise function between the given lower and higher bound
clampNoise :: (Ord a) => a -> a -> Noise p a -> Noise p a
clampNoise l u = fmap (clamp l u)
{-# INLINE clampNoise #-}

sliceX2 :: p -> Noise2' p v -> Noise1' p v
sliceX2 x = warp (x,)
{-# INLINE sliceX2 #-}

sliceY2 :: p -> Noise2' p v -> Noise1' p v
sliceY2 y = warp (,y)
{-# INLINE sliceY2 #-}

sliceX3 :: p -> Noise3' p v -> Noise2' p v
sliceX3 x = warp (\(y, z) -> (x, y, z))
{-# INLINE sliceX3 #-}

sliceY3 :: p -> Noise3' p v -> Noise2' p v
sliceY3 y = warp (\(x, z) -> (x, y, z))
{-# INLINE sliceY3 #-}

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
