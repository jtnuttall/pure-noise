{-# LANGUAGE Strict #-}

-- |
-- Maintainer: Jeremy Nuttall <jeremy@jeremy-nuttall.com>
-- Stability: experimental
--
-- This module implements a variation of OpenSimplex2 noise derived from FastNoiseLite.
-- See openSimplex2S
module Numeric.Noise.SuperSimplex (
  -- * 2D Noise
  noise2,
  noise2Base,
) where

import Data.Bits
import Numeric.Noise.Internal
import Numeric.Noise.Internal.Math

noise2 :: (RealFrac a) => Noise2 a
noise2 = Noise2 noise2Base
{-# INLINE noise2 #-}

noise2Base :: (RealFrac a) => Seed -> a -> a -> a
noise2Base seed xo yo =
  let f2 = 0.5 * (sqrt3 - 1)
      to = (xo + yo) * f2
      x = xo + to
      y = yo + to

      fx = floor x
      fy = floor y
      xi = x - fromIntegral @Hash fx
      yi = y - fromIntegral @Hash fy

      i = fx * primeX
      j = fy * primeY
      i1 = i + primeX
      j1 = j + primeY

      t = (xi + yi) * g2
      x0 = xi - t
      y0 = yi - t

      a0 = (2 / 3) - x0 * x0 - y0 * y0
      v0 = (a0 * a0) * (a0 * a0) * gradCoord2 seed i j x0 y0

      v1 =
        let g2t = 1 - 2 * g2
            a1 =
              (2 * g2t * (1 / g2 - 2)) * t
                + ((-2 * g2t * g2t) + a0)
            x1 = x0 - g2t
            y1 = y0 - g2t
         in (a1 * a1) * (a1 * a1) * gradCoord2 seed i1 j1 x1 y1

      xmyi = xi - yi

      ~vgx
        | xi + xmyi > 1 =
            let ~x2 = x0 + (3 * g2 - 2)
                ~y2 = y0 + (3 * g2 - 1)
                ~a2 = (2 / 3) - x2 * x2 - y2 * y2
             in attenuate a2 seed (i + (primeX `shiftL` 1)) (j + primeY) x2 y2
        | otherwise =
            let ~x2 = x0 + g2
                ~y2 = y0 + (g2 - 1)
                ~a2 = (2 / 3) - x2 * x2 - y2 * y2
             in attenuate a2 seed i (j + primeY) x2 y2

      ~vgy
        | yi - xmyi > 1 =
            let ~x3 = x0 + (3 * g2 - 1)
                ~y3 = y0 + (3 * g2 - 2)
                ~a3 = (2 / 3) - x3 * x3 - y3 * y3
             in attenuate a3 seed (i + primeX) (j + (primeY `shiftL` 1)) x3 y3
        | otherwise =
            let ~x3 = x0 + (g2 - 1)
                ~y3 = y0 + g2
                ~a3 = (2 / 3) - x3 * x3 - y3 * y3
             in attenuate a3 seed (i + primeX) j x3 y3

      ~vlx
        | xi + xmyi < 0 =
            let ~x2 = x0 + (1 - g2)
                ~y2 = y0 - g2
                ~a2 = (2 / 3) - x2 * x2 - y2 * y2
             in attenuate a2 seed (i - primeX) j x2 y2
        | otherwise =
            let ~x2 = x0 + (g2 - 1)
                ~y2 = y0 + g2
                ~a2 = (2 / 3) - x2 * x2 - y2 * y2
             in attenuate a2 seed (i + primeX) j x2 y2
      ~vly
        | yi < xmyi =
            let ~x2 = x0 - g2
                ~y2 = y0 - (g2 - 1)
                ~a2 = (2 / 3) - x2 * x2 - y2 * y2
             in attenuate a2 seed i (j - primeY) x2 y2
        | otherwise =
            let ~x2 = x0 + g2
                ~y2 = y0 + (g2 - 1)
                ~a2 = (2 / 3) - x2 * x2 - y2 * y2
             in attenuate a2 seed i (j + primeY) x2 y2

      v2
        | t > g2 = vgx + vgy
        | otherwise = vlx + vly
   in normalize $ v0 + v1 + v2
{-# INLINE noise2Base #-}

attenuate :: (RealFrac a) => a -> Seed -> Hash -> Hash -> a -> a -> a
attenuate !vi !seed !i !j !x !y =
  let !v = max 0 vi
   in (v * v) * (v * v) * gradCoord2 seed i j x y
{-# INLINE attenuate #-}

normalize :: (RealFrac a) => a -> a
normalize = (18.24196194486065 *)
{-# INLINE normalize #-}
