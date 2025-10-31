-- |
-- Maintainer: Jeremy Nuttall <jeremy@jeremy-nuttall.com>
-- Stability: experimental
--
-- This module implements a variation of OpenSimplex2 noise derived from FastNoiseLite.
module Numeric.Noise.OpenSimplex (
  -- * 2D Noise
  noise2,
  noise2Base,
) where

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
        let cond = bool 0 1 (y0 > x0)
            x1 = (x0 + g2 - 1) + fromIntegral cond
            y1 = (y0 + g2) - fromIntegral cond
            i1 = i + (1 - cond) * primeX
            j1 = j + cond * primeY
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
