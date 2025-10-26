{-# LANGUAGE OverloadedLists #-}

-- |
-- Maintainer: Jeremy Nuttall <jeremy@jeremy-nuttall.com>
-- License: BSD-3-Clause
-- Stability : experimental
module Numeric.Noise.Internal.Math (
  Seed,
  Hash,
  lerp,
  cubicInterp,
  hermiteInterp,
  quinticInterp,
  clamp,
  primeX,
  primeY,
  primeZ,
  hash2,
  hash3,
  infinity,
  g2,
  sqrt3,
  valCoord2,
  valCoord3,
  gradCoord2,
  gradCoord3,
  maxHash,
) where

import Data.Bits
import Data.Int
import Data.Primitive.PrimArray
import Data.Word

-- | Seed value for deterministic noise generation.
--
-- Using the same 'Seed' value will produce the same noise pattern,
-- allowing for reproducible results. Different seed values produce
-- different, independent noise patterns.
type Seed = Word64

-- | Internal hash value type used in noise calculations.
type Hash = Int32

-- | Linear interpolation between two values.
--
-- Monotonic lerp
lerp
  :: (Num a)
  => a
  -- ^ start
  -> a
  -- ^ end
  -> a
  -- ^ parameter in range [0, 1]
  -> a
lerp v0 v1 t = v0 * (1 - t) + (v1 * t)
{-# INLINE [0] lerp #-}

{-# RULES
"lerp/Float/0" forall (a :: Float) (b :: Float). lerp a b 0 = a
"lerp/Double/0" forall (a :: Double) (b :: Double). lerp a b 0 = a
"lerp/Float/1" forall (a :: Float) (b :: Float). lerp a b 1 = b
"lerp/Double/1" forall (a :: Double) (b :: Double). lerp a b 1 = b
"lerp/Float/id" forall (t :: Float) (a :: Float). lerp a a t = a
"lerp/Double/id" forall (a :: Double) (t :: Double). lerp a a t = a
"lerp/compose/start" forall a b t u.
  lerp (lerp a b u) b t =
    lerp a b (u + t - t * u)
"lerp/compose/end" forall a b t u.
  lerp a (lerp a b u) t =
    lerp a b (t * u)
  #-}

-- | cubic interpolation
cubicInterp :: (Num a) => a -> a -> a -> a -> a -> a
cubicInterp a b c d t =
  let !p = (d - c) - (a - b)
   in t * t * t * p + t * t * ((a - b) - p) + t * (c - a) + b
{-# INLINEABLE cubicInterp #-}

-- | hermite interpolation
hermiteInterp :: (Num a) => a -> a
hermiteInterp t = t * t * (3 - 2 * t)
{-# INLINEABLE hermiteInterp #-}

-- | quintic interpolation
quinticInterp :: (Num a) => a -> a
quinticInterp t = t * t * t * (t * (t * 6 - 15) + 10)
{-# INLINEABLE quinticInterp #-}

-- | Clamp a value to a specified range.
--
-- Returns the value if it's within bounds, otherwise returns
-- the nearest boundary.
clamp
  :: (Ord a)
  => a
  -- ^ lower bound
  -> a
  -- ^ upper bound
  -> a
  -- ^ value
  -> a
clamp l u v = min (max v l) u
{-# INLINE clamp #-}

primeX, primeY, primeZ :: Hash
primeX = 501125321
{-# INLINE primeX #-}
primeY = 1136930381
{-# INLINE primeY #-}
primeZ = 1720413743
{-# INLINE primeZ #-}

hash2 :: Seed -> Hash -> Hash -> Hash
hash2 seed xPrimed yPrimed =
  (fromIntegral seed `xor` xPrimed `xor` yPrimed)
    * 0x27d4eb2d
{-# INLINE hash2 #-}

hash3 :: Seed -> Hash -> Hash -> Hash -> Hash
hash3 seed xPrimed yPrimed zPrimed =
  (fromIntegral seed `xor` xPrimed `xor` yPrimed `xor` zPrimed)
    * 0x27d4eb2d
{-# INLINE hash3 #-}

infinity :: (Fractional a) => a
infinity = 1 / 0
{-# INLINE infinity #-}

g2 :: (Fractional a) => a
g2 = (3 - sqrt3) / 6
{-# INLINE g2 #-}

sqrt3 :: (Fractional a) => a
sqrt3 = 1.7320508075688772935274463415059
{-# INLINE sqrt3 #-}

valCoord2 :: (RealFrac a) => Seed -> Hash -> Hash -> a
valCoord2 seed xPrimed yPrimed =
  let !hash = hash2 seed xPrimed yPrimed
      !val = (hash * hash) `xor` (hash `shiftL` 19)
   in fromIntegral val / maxHash
{-# INLINEABLE valCoord2 #-}

valCoord3 :: (RealFrac a) => Seed -> Hash -> Hash -> Hash -> a
valCoord3 seed xPrimed yPrimed zPrimed =
  let !hash = hash3 seed xPrimed yPrimed zPrimed
      !val = (hash * hash) `xor` (hash `shiftL` 19)
   in fromIntegral val / maxHash
{-# INLINE valCoord3 #-}

gradCoord2 :: (RealFrac a) => Seed -> Hash -> Hash -> a -> a -> a
gradCoord2 seed xPrimed yPrimed xd yd =
  let !hash = hash2 seed xPrimed yPrimed
      !ix = (hash `xor` (hash `shiftR` 15)) .&. 0xFE
      !xg = lookupGrad2 ix
      !yg = lookupGrad2 (ix .|. 1)
   in xd * xg + yd * yg
{-# INLINE [2] gradCoord2 #-}

gradCoord3 :: (RealFrac a) => Seed -> Hash -> Hash -> Hash -> a -> a -> a -> a
gradCoord3 seed xPrimed yPrimed zPrimed xd yd zd =
  let !hash = hash3 seed xPrimed yPrimed zPrimed
      !ix = (hash `xor` (hash `shiftR` 15)) .&. 0xFC
      !xg = grad3d `indexPrimArray` fromIntegral ix
      !yg = grad3d `indexPrimArray` fromIntegral (ix .|. 1)
      !zg = grad3d `indexPrimArray` fromIntegral (ix .|. 2)
   in xd * fromIntegral xg + yd * fromIntegral yg + zd * fromIntegral zg
{-# INLINE gradCoord3 #-}

maxHash :: (RealFrac a) => a
maxHash = realToFrac (maxBound @Hash)
{-# INLINE maxHash #-}

lookupGrad2 :: (RealFrac a) => Hash -> a
lookupGrad2 = realToFrac . (grad2dd `indexPrimArray`) . fromIntegral
{-# INLINE [0] lookupGrad2 #-}

{-# RULES
"lookupGrad2/Float" forall (i :: Hash).
  lookupGrad2 i =
    indexPrimArray grad2df (fromIntegral i)
"lookupGrad2/Double" forall (i :: Hash).
  lookupGrad2 i =
    indexPrimArray grad2dd (fromIntegral i)
  #-}

grad2df :: PrimArray Float
grad2df = mapPrimArray realToFrac grad2dd

{- ORMOLU_DISABLE -}
-- >>> sizeofPrimArray grad2d == 256
-- True
grad2dd :: PrimArray Double
grad2dd =
  [ 0.130526192220052,  0.99144486137381 ,  0.38268343236509 ,  0.923879532511287,  0.608761429008721,  0.793353340291235,  0.793353340291235,  0.608761429008721,
    0.923879532511287,  0.38268343236509 ,  0.99144486137381 ,  0.130526192220051,  0.99144486137381 , -0.130526192220051,  0.923879532511287, -0.38268343236509,
    0.793353340291235, -0.60876142900872 ,  0.608761429008721, -0.793353340291235,  0.38268343236509 , -0.923879532511287,  0.130526192220052, -0.99144486137381,
   -0.130526192220052, -0.99144486137381 , -0.38268343236509 , -0.923879532511287, -0.608761429008721, -0.793353340291235, -0.793353340291235, -0.608761429008721,
   -0.923879532511287, -0.38268343236509 , -0.99144486137381 , -0.130526192220052, -0.99144486137381 ,  0.130526192220051, -0.923879532511287,  0.38268343236509,
   -0.793353340291235,  0.608761429008721, -0.608761429008721,  0.793353340291235, -0.38268343236509 ,  0.923879532511287, -0.130526192220052,  0.99144486137381,
    0.130526192220052,  0.99144486137381 ,  0.38268343236509 ,  0.923879532511287,  0.608761429008721,  0.793353340291235,  0.793353340291235,  0.608761429008721,
    0.923879532511287,  0.38268343236509 ,  0.99144486137381 ,  0.130526192220051,  0.99144486137381 , -0.130526192220051,  0.923879532511287, -0.38268343236509,
    0.793353340291235, -0.60876142900872 ,  0.608761429008721, -0.793353340291235,  0.38268343236509 , -0.923879532511287,  0.130526192220052, -0.99144486137381,
   -0.130526192220052, -0.99144486137381 , -0.38268343236509 , -0.923879532511287, -0.608761429008721, -0.793353340291235, -0.793353340291235, -0.608761429008721,
   -0.923879532511287, -0.38268343236509 , -0.99144486137381 , -0.130526192220052, -0.99144486137381 ,  0.130526192220051, -0.923879532511287,  0.38268343236509,
   -0.793353340291235,  0.608761429008721, -0.608761429008721,  0.793353340291235, -0.38268343236509 ,  0.923879532511287, -0.130526192220052,  0.99144486137381,
    0.130526192220052,  0.99144486137381 ,  0.38268343236509 ,  0.923879532511287,  0.608761429008721,  0.793353340291235,  0.793353340291235,  0.608761429008721,
    0.923879532511287,  0.38268343236509 ,  0.99144486137381 ,  0.130526192220051,  0.99144486137381 , -0.130526192220051,  0.923879532511287, -0.38268343236509,
    0.793353340291235, -0.60876142900872 ,  0.608761429008721, -0.793353340291235,  0.38268343236509 , -0.923879532511287,  0.130526192220052, -0.99144486137381,
   -0.130526192220052, -0.99144486137381 , -0.38268343236509 , -0.923879532511287, -0.608761429008721, -0.793353340291235, -0.793353340291235, -0.608761429008721,
   -0.923879532511287, -0.38268343236509 , -0.99144486137381 , -0.130526192220052, -0.99144486137381 ,  0.130526192220051, -0.923879532511287,  0.38268343236509,
   -0.793353340291235,  0.608761429008721, -0.608761429008721,  0.793353340291235, -0.38268343236509 ,  0.923879532511287, -0.130526192220052,  0.99144486137381,
    0.130526192220052,  0.99144486137381 ,  0.38268343236509 ,  0.923879532511287,  0.608761429008721,  0.793353340291235,  0.793353340291235,  0.608761429008721,
    0.923879532511287,  0.38268343236509 ,  0.99144486137381 ,  0.130526192220051,  0.99144486137381 , -0.130526192220051,  0.923879532511287, -0.38268343236509,
    0.793353340291235, -0.60876142900872 ,  0.608761429008721, -0.793353340291235,  0.38268343236509 , -0.923879532511287,  0.130526192220052, -0.99144486137381,
   -0.130526192220052, -0.99144486137381 , -0.38268343236509 , -0.923879532511287, -0.608761429008721, -0.793353340291235, -0.793353340291235, -0.608761429008721,
   -0.923879532511287, -0.38268343236509 , -0.99144486137381 , -0.130526192220052, -0.99144486137381 ,  0.130526192220051, -0.923879532511287,  0.38268343236509,
   -0.793353340291235,  0.608761429008721, -0.608761429008721,  0.793353340291235, -0.38268343236509 ,  0.923879532511287, -0.130526192220052,  0.99144486137381,
    0.130526192220052,  0.99144486137381 ,  0.38268343236509 ,  0.923879532511287,  0.608761429008721,  0.793353340291235,  0.793353340291235,  0.608761429008721,
    0.923879532511287,  0.38268343236509 ,  0.99144486137381 ,  0.130526192220051,  0.99144486137381 , -0.130526192220051,  0.923879532511287, -0.38268343236509,
    0.793353340291235, -0.60876142900872 ,  0.608761429008721, -0.793353340291235,  0.38268343236509 , -0.923879532511287,  0.130526192220052, -0.99144486137381,
   -0.130526192220052, -0.99144486137381 , -0.38268343236509 , -0.923879532511287, -0.608761429008721, -0.793353340291235, -0.793353340291235, -0.608761429008721,
   -0.923879532511287, -0.38268343236509 , -0.99144486137381 , -0.130526192220052, -0.99144486137381 ,  0.130526192220051, -0.923879532511287,  0.38268343236509,
   -0.793353340291235,  0.608761429008721, -0.608761429008721,  0.793353340291235, -0.38268343236509 ,  0.923879532511287, -0.130526192220052,  0.99144486137381,
    0.38268343236509 ,  0.923879532511287,  0.923879532511287,  0.38268343236509 ,  0.923879532511287, -0.38268343236509 ,  0.38268343236509 , -0.923879532511287,
   -0.38268343236509 , -0.923879532511287, -0.923879532511287, -0.38268343236509 , -0.923879532511287,  0.38268343236509 , -0.38268343236509 ,  0.923879532511287
  ]

-- >>> sizeofPrimArray grad3d == 256
-- True
grad3d :: PrimArray Int
grad3d =
  [ 0, 1, 1, 0, 0, -1, 1, 0, 0, 1, -1, 0, 0, -1, -1, 0
  , 1, 0, 1, 0, -1, 0, 1, 0, 1, 0, -1, 0, -1, 0, -1, 0
  , 1, 1, 0, 0, -1, 1, 0, 0, 1, -1, 0, 0, -1, -1, 0, 0
  , 0, 1, 1, 0, 0, -1, 1, 0, 0, 1, -1, 0, 0, -1, -1, 0
  , 1, 0, 1, 0, -1, 0, 1, 0, 1, 0, -1, 0, -1, 0, -1, 0
  , 1, 1, 0, 0, -1, 1, 0, 0, 1, -1, 0, 0, -1, -1, 0, 0
  , 0, 1, 1, 0, 0, -1, 1, 0, 0, 1, -1, 0, 0, -1, -1, 0
  , 1, 0, 1, 0, -1, 0, 1, 0, 1, 0, -1, 0, -1, 0, -1, 0
  , 1, 1, 0, 0, -1, 1, 0, 0, 1, -1, 0, 0, -1, -1, 0, 0
  , 0, 1, 1, 0, 0, -1, 1, 0, 0, 1, -1, 0, 0, -1, -1, 0
  , 1, 0, 1, 0, -1, 0, 1, 0, 1, 0, -1, 0, -1, 0, -1, 0
  , 1, 1, 0, 0, -1, 1, 0, 0, 1, -1, 0, 0, -1, -1, 0, 0
  , 0, 1, 1, 0, 0, -1, 1, 0, 0, 1, -1, 0, 0, -1, -1, 0
  , 1, 0, 1, 0, -1, 0, 1, 0, 1, 0, -1, 0, -1, 0, -1, 0
  , 1, 1, 0, 0, -1, 1, 0, 0, 1, -1, 0, 0, -1, -1, 0, 0
  , 1, 1, 0, 0, 0, -1, 1, 0, -1, 1, 0, 0, 0, -1, -1, 0
  ]
