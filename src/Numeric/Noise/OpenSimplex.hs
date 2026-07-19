-- |
-- Maintainer: Jeremy Nuttall <jeremy@jeremy-nuttall.com>
-- Stability: experimental
--
-- This module implements a variation of OpenSimplex2 noise derived from FastNoiseLite.
module Numeric.Noise.OpenSimplex (
  -- * 2D Noise
  noise2,
  noise2Base,

  -- * 3D Noise
  noise3,
  noise3Base,
) where

import Data.Bits (complement, shiftR, (.&.))
import Data.Bool (bool)
import Numeric.Noise.Internal
import Numeric.Noise.Internal.Math

noise2 :: (RealFrac a) => Noise2 a
noise2 = mkNoise2 noise2Base
{-# INLINE noise2 #-}

noise2Base :: (RealFrac a) => Seed -> a -> a -> a
noise2Base seed xo yo =
  let !f2 = 0.5 * (sqrt3 - 1)
      !to = (xo + yo) * f2
      !x = xo + to
      !y = yo + to

      !fx = floor x
      !fy = floor y
      !xi = x - fromIntegral fx
      !yi = y - fromIntegral fy

      !t = (xi + yi) * g2
      !x0 = xi - t
      !y0 = yi - t
      !i = fx * primeX
      !j = fy * primeY

      !a = 0.5 - x0 * x0 - y0 * y0
      n0 = attenuate a seed i j x0 y0

      n1 =
        let gt = y0 > x0
            -- Select the constant-folded addend per branch (g2 or g2 - 1),
            -- reproducing FNL's `x0 + (float)G2` / `x0 + ((float)G2 - 1)`
            -- rounding. Arithmetic on the selector, e.g.
            -- (x0 + g2 - 1) + cond, rounds differently when |x0 + g2| < 1.
            x1 = x0 + bool (g2 - 1) g2 gt
            y1 = y0 + bool g2 (g2 - 1) gt
            i1 = i + bool primeX 0 gt
            j1 = j + bool 0 primeY gt
            b = 0.5 - x1 * x1 - y1 * y1
         in attenuate b seed i1 j1 x1 y1

      !n2 =
        let g2t = 1 - 2 * g2
            c = 2 * g2t * (1 / g2 - 2) * t + (-2 * g2t * g2t + a)
            x2 = x0 + (2 * g2 - 1)
            y2 = y0 + (2 * g2 - 1)
         in attenuate c seed (i + primeX) (j + primeY) x2 y2
   in normalize $ n0 + n1 + n2
{-# INLINE [2] noise2Base #-}

attenuate :: (RealFrac a) => a -> Seed -> Hash -> Hash -> a -> a -> a
attenuate !vi seed i j x y =
  let !v = max 0 vi
   in (v * v) * (v * v) * gradCoord2 seed i j x y
{-# INLINE attenuate #-}

normalize :: (RealFrac a) => a -> a
normalize = (99.83685446303647 *)
{-# INLINE normalize #-}

noise3 :: (RealFrac a) => Noise3 a
noise3 = mkNoise3 noise3Base
{-# INLINE noise3 #-}

noise3Base :: forall a. (RealFrac a) => Seed -> a -> a -> a -> a
noise3Base seed0 xo yo zo =
  let !(x, y, z) = rotate3 xo yo zo

      !i0 = fastRound x
      !j0 = fastRound y
      !k0 = fastRound z
      !x0 = x - fromIntegral i0
      !y0 = y - fromIntegral j0
      !z0 = z - fromIntegral k0

      -- FNL: (int)(-1.0f - x0) | 1, i.e. -1 when the offset is >= 0
      !xns = bool 1 (-1) (x0 >= 0) :: Hash
      !yns = bool 1 (-1) (y0 >= 0) :: Hash
      !zns = bool 1 (-1) (z0 >= 0) :: Hash

      !ax0 = fromIntegral xns * negate x0
      !ay0 = fromIntegral yns * negate y0
      !az0 = fromIntegral zns * negate z0

      !ip = i0 * primeX
      !jp = j0 * primeY
      !kp = k0 * primeZ

      !a0 = (0.6 - x0 * x0) - (y0 * y0 + z0 * z0)

      !v0 = iteration seed0 a0 ip jp kp x0 y0 z0 ax0 ay0 az0 xns yns zns

      -- second lattice: reflect offsets, flip signs, complement the seed
      !ax1 = 0.5 - ax0
      !ay1 = 0.5 - ay0
      !az1 = 0.5 - az0
      !x1 = fromIntegral xns * ax1
      !y1 = fromIntegral yns * ay1
      !z1 = fromIntegral zns * az1
      !a1 = a0 + ((0.75 - ax1) - (ay1 + az1))
      !ip' = ip + ((xns `shiftR` 1) .&. primeX)
      !jp' = jp + ((yns `shiftR` 1) .&. primeY)
      !kp' = kp + ((zns `shiftR` 1) .&. primeZ)

      !v1 = iteration (complement seed0) a1 ip' jp' kp' x1 y1 z1 ax1 ay1 az1 (negate xns) (negate yns) (negate zns)
   in (v0 + v1) * 32.69428253173828125
 where
  -- one loop iteration of the FNL reference: the cell contribution plus one
  -- step along the dominant axis
  iteration
    :: Seed -> a -> Hash -> Hash -> Hash -> a -> a -> a -> a -> a -> a -> Hash -> Hash -> Hash -> a
  iteration !sd !a !i !j !k !x0 !y0 !z0 !ax !ay !az !xns !yns !zns =
    let !va
          | a > 0 = (a * a) * (a * a) * gradCoord3 sd i j k x0 y0 z0
          | otherwise = 0
        !b0 = a + 1
        !vb
          | ax >= ay && ax >= az =
              let !x1 = x0 + fromIntegral xns
                  !b = b0 - fromIntegral (xns * 2) * x1
               in bContrib sd b (i - xns * primeX) j k x1 y0 z0
          | ay > ax && ay >= az =
              let !y1 = y0 + fromIntegral yns
                  !b = b0 - fromIntegral (yns * 2) * y1
               in bContrib sd b i (j - yns * primeY) k x0 y1 z0
          | otherwise =
              let !z1 = z0 + fromIntegral zns
                  !b = b0 - fromIntegral (zns * 2) * z1
               in bContrib sd b i j (k - zns * primeZ) x0 y0 z1
     in va + vb
  {-# INLINE iteration #-}

  bContrib :: Seed -> a -> Hash -> Hash -> Hash -> a -> a -> a -> a
  bContrib !sd !b !i !j !k !x1 !y1 !z1
    | b > 0 = (b * b) * (b * b) * gradCoord3 sd i j k x1 y1 z1
    | otherwise = 0
  {-# INLINE bContrib #-}
{-# INLINE [2] noise3Base #-}
