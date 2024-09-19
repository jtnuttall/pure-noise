{-# LANGUAGE Strict #-}

-- |
-- Maintainer: Jeremy Nuttall <jeremy@jeremy-nuttall.com>
-- Stability: experimental
--
-- This module implements a variation of value noise derived from FastNoiseLite.
module Numeric.Noise.Value (
  -- * 2D Noise
  noise2,
  noise2Base,

  -- * 3D Noise
  noise3,
  noise3Base,
)
where

import Numeric.Noise.Internal
import Numeric.Noise.Internal.Math

noise2 :: (RealFrac a) => Noise2 a
noise2 = Noise2 noise2Base
{-# INLINE noise2 #-}

noise2Base :: (RealFrac a) => Seed -> a -> a -> a
noise2Base seed x y =
  let x0 = floor x
      y0 = floor y

      xs = hermiteInterp (x - fromIntegral x0)
      ys = hermiteInterp (y - fromIntegral y0)

      x0p = x0 * primeX
      y0p = y0 * primeY

      x1 = x0p + primeX
      y1 = y0p + primeY
   in lerp
        ( lerp
            (valCoord2 seed x0p y0p)
            (valCoord2 seed x1 y0p)
            xs
        )
        ( lerp
            (valCoord2 seed x0p y1)
            (valCoord2 seed x1 y1)
            xs
        )
        ys
{-# INLINE noise2Base #-}

noise3 :: (RealFrac a) => Noise3 a
noise3 = Noise3 noise3Base
{-# INLINE noise3 #-}

noise3Base :: (RealFrac a) => Seed -> a -> a -> a -> a
noise3Base seed x y z =
  let x0 = floor x
      y0 = floor y
      z0 = floor z

      xs = hermiteInterp (x - fromIntegral x0)
      ys = hermiteInterp (y - fromIntegral y0)
      zs = hermiteInterp (z - fromIntegral z0)

      x0p = x0 * primeX
      y0p = y0 * primeY
      z0p = z0 * primeZ

      x1 = x0p + primeX
      y1 = y0p + primeY
      z1 = z0p + primeZ
   in lerp
        ( lerp
            ( lerp
                (valCoord3 seed x0p y0p z0p)
                (valCoord3 seed x1 y0p z0p)
                xs
            )
            ( lerp
                (valCoord3 seed x0p y1 z0p)
                (valCoord3 seed x1 y1 z0p)
                xs
            )
            ys
        )
        ( lerp
            ( lerp
                (valCoord3 seed x0p y0p z1)
                (valCoord3 seed x1 y0p z1)
                xs
            )
            ( lerp
                (valCoord3 seed x0p y1 z1)
                (valCoord3 seed x1 y1 z1)
                xs
            )
            ys
        )
        zs
{-# INLINE noise3Base #-}