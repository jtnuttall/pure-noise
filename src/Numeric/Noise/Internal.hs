-- |
-- Maintainer: Jeremy Nuttall <jeremy@jeremy-nuttall.com>
-- Stability : experimental
module Numeric.Noise.Internal (
  module Math,
  Noise2 (..),
  map2,
  clamp2,
  const2,
  Noise3 (..),
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

newtype Noise2 a = Noise2
  {unNoise2 :: Seed -> a -> a -> a}

map2 :: (a -> a) -> Noise2 a -> Noise2 a
map2 f (Noise2 g) = Noise2 (\s x y -> f (g s x y))
{-# INLINE map2 #-}

clamp2 :: (Ord a) => a -> a -> Noise2 a -> Noise2 a
clamp2 l u (Noise2 f) = Noise2 $ \s x y -> clamp l u (f s x y)
{-# INLINE clamp2 #-}

const2 :: a -> Noise2 a
const2 a = Noise2 (\_ _ _ -> a)
{-# INLINE const2 #-}

instance (Num a) => Num (Noise2 a) where
  Noise2 f + Noise2 g = Noise2 $ \s x y -> f s x y + g s x y
  {-# INLINE (+) #-}
  Noise2 f * Noise2 g = Noise2 $ \s x y -> f s x y * g s x y
  {-# INLINE (*) #-}
  abs (Noise2 f) = Noise2 $ \s x y -> abs (f s x y)
  {-# INLINE abs #-}
  signum (Noise2 f) = Noise2 $ \s x y -> signum (f s x y)
  {-# INLINE signum #-}
  fromInteger i = const2 (fromInteger i)
  {-# INLINE fromInteger #-}
  negate (Noise2 f) = Noise2 $ \s x y -> negate (f s x y)
  {-# INLINE negate #-}

instance (Fractional a) => Fractional (Noise2 a) where
  fromRational r = const2 (fromRational r)
  {-# INLINE fromRational #-}
  recip (Noise2 f) = Noise2 $ \s x y -> recip (f s x y)
  {-# INLINE recip #-}
  Noise2 f / Noise2 g = Noise2 $ \s x y -> f s x y / g s x y
  {-# INLINE (/) #-}

instance (Floating a) => Floating (Noise2 a) where
  pi = const2 pi
  {-# INLINE pi #-}
  exp (Noise2 f) = Noise2 $ \s x y -> exp (f s x y)
  {-# INLINE exp #-}
  log (Noise2 f) = Noise2 $ \s x y -> log (f s x y)
  {-# INLINE log #-}
  sin (Noise2 f) = Noise2 $ \s x y -> sin (f s x y)
  {-# INLINE sin #-}
  cos (Noise2 f) = Noise2 $ \s x y -> cos (f s x y)
  {-# INLINE cos #-}
  asin (Noise2 f) = Noise2 $ \s x y -> asin (f s x y)
  {-# INLINE asin #-}
  acos (Noise2 f) = Noise2 $ \s x y -> acos (f s x y)
  {-# INLINE acos #-}
  atan (Noise2 f) = Noise2 $ \s x y -> atan (f s x y)
  {-# INLINE atan #-}
  sinh (Noise2 f) = Noise2 $ \s x y -> sinh (f s x y)
  {-# INLINE sinh #-}
  cosh (Noise2 f) = Noise2 $ \s x y -> cosh (f s x y)
  {-# INLINE cosh #-}
  asinh (Noise2 f) = Noise2 $ \s x y -> asinh (f s x y)
  {-# INLINE asinh #-}
  acosh (Noise2 f) = Noise2 $ \s x y -> acosh (f s x y)
  {-# INLINE acosh #-}
  atanh (Noise2 f) = Noise2 $ \s x y -> atanh (f s x y)
  {-# INLINE atanh #-}

newtype Noise3 a = Noise3
  {unNoise3 :: Seed -> a -> a -> a -> a}

map3 :: (a -> a) -> Noise3 a -> Noise3 a
map3 f (Noise3 g) = Noise3 (\s x y z -> f (g s x y z))
{-# INLINE map3 #-}

const3 :: a -> Noise3 a
const3 a = Noise3 (\_ _ _ _ -> a)
{-# INLINE const3 #-}

clamp3 :: (Ord a) => a -> a -> Noise3 a -> Noise3 a
clamp3 l u (Noise3 f) = Noise3 $ \s x y z -> clamp l u (f s x y z)
{-# INLINE clamp3 #-}

instance (Num a) => Num (Noise3 a) where
  Noise3 f + Noise3 g = Noise3 $ \s x y z -> f s x y z + g s x y z
  {-# INLINE (+) #-}
  Noise3 f * Noise3 g = Noise3 $ \s x y z -> f s x y z * g s x y z
  {-# INLINE (*) #-}
  abs (Noise3 f) = Noise3 $ \s x y z -> abs (f s x y z)
  {-# INLINE abs #-}
  signum (Noise3 f) = Noise3 $ \s x y z -> signum (f s x y z)
  {-# INLINE signum #-}
  fromInteger i = const3 (fromInteger i)
  {-# INLINE fromInteger #-}
  negate (Noise3 f) = Noise3 $ \s x y z -> negate (f s x y z)
  {-# INLINE negate #-}

instance (Fractional a) => Fractional (Noise3 a) where
  fromRational r = const3 (fromRational r)
  {-# INLINE fromRational #-}
  recip (Noise3 f) = Noise3 $ \s x y z -> recip (f s x y z)
  {-# INLINE recip #-}

instance (Floating a) => Floating (Noise3 a) where
  pi = const3 pi
  {-# INLINE pi #-}
  exp (Noise3 f) = Noise3 $ \s x y z -> exp (f s x y z)
  {-# INLINE exp #-}
  log (Noise3 f) = Noise3 $ \s x y z -> log (f s x y z)
  {-# INLINE log #-}
  sin (Noise3 f) = Noise3 $ \s x y z -> sin (f s x y z)
  {-# INLINE sin #-}
  cos (Noise3 f) = Noise3 $ \s x y z -> cos (f s x y z)
  {-# INLINE cos #-}
  asin (Noise3 f) = Noise3 $ \s x y z -> asin (f s x y z)
  {-# INLINE asin #-}
  acos (Noise3 f) = Noise3 $ \s x y z -> acos (f s x y z)
  {-# INLINE acos #-}
  atan (Noise3 f) = Noise3 $ \s x y z -> atan (f s x y z)
  {-# INLINE atan #-}
  sinh (Noise3 f) = Noise3 $ \s x y z -> sinh (f s x y z)
  {-# INLINE sinh #-}
  cosh (Noise3 f) = Noise3 $ \s x y z -> cosh (f s x y z)
  {-# INLINE cosh #-}
  asinh (Noise3 f) = Noise3 $ \s x y z -> asinh (f s x y z)
  {-# INLINE asinh #-}
  acosh (Noise3 f) = Noise3 $ \s x y z -> acosh (f s x y z)
  {-# INLINE acosh #-}
  atanh (Noise3 f) = Noise3 $ \s x y z -> atanh (f s x y z)
  {-# INLINE atanh #-}
