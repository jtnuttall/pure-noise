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

import Numeric.Noise.Internal
import Numeric.Noise.Internal.Math

noise2 :: (RealFrac a) => Noise2 a
noise2 = Noise2 noise2Base
{-# INLINE noise2 #-}

noise2Base :: (RealFrac a) => Seed -> a -> a -> a
noise2Base !seed !xo !yo =
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
      n0 = attenuate a i j x0 y0

      {-# INLINE n1 #-}
      n1
        | y0 > x0 =
            let x1 = x0 + g2
                y1 = y0 + (g2 - 1)
                j1 = j + primeY
                b = 0.5 - x1 * x1 - y1 * y1
             in attenuate b i j1 x1 y1
        | otherwise =
            let x1 = x0 + (g2 - 1)
                i1 = i + primeX
                y1 = y0 + g2
                b = 0.5 - x1 * x1 - y1 * y1
             in attenuate b i1 j x1 y1

      !n2 =
        let g2t = 1 - 2 * g2
            c = 2 * g2t * (1 / g2 - 2) * t + (-2 * g2t * g2t + a)
            x2 = x0 + (2 * g2 - 1)
            y2 = y0 + (2 * g2 - 1)
         in attenuate c (i + primeX) (j + primeY) x2 y2
   in normalize $ n0 + n1 + n2
 where
  attenuate :: (RealFrac a) => a -> Hash -> Hash -> a -> a -> a
  attenuate !v i j x y
    | v <= 0 = 0
    | otherwise = (v * v) * (v * v) * gradCoord2 seed i j x y
{-# INLINE noise2Base #-}

normalize :: (RealFrac a) => a -> a
normalize = (99.83685446303647 *)
{-# INLINE normalize #-}
