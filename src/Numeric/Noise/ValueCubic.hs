-- |
-- Maintainer: Jeremy Nuttall <jeremy@jeremy-nuttall.com>
-- Stability : experimental
module Numeric.Noise.ValueCubic (
  -- * 2D Noise
  noise2,
  noise2Base,

  -- * 3D Noise
  noise3,
  noise3Base,
) where

import Data.Bits
import Numeric.Noise.Internal
import Numeric.Noise.Internal.Math

noise2 :: (RealFrac a) => Noise2 a
noise2 = Noise2 noise2Base
{-# INLINE noise2 #-}

noise2Base :: (RealFrac a) => Seed -> a -> a -> a
noise2Base seed x y =
  let x1f = floor x
      y1f = floor y

      xs = x - fromIntegral x1f
      ys = y - fromIntegral y1f

      x1 = x1f * primeX
      y1 = y1f * primeY
      x0 = x1 - primeX
      y0 = y1 - primeY
      x2 = x1 + primeX
      y2 = y1 + primeY
      x3 = x1 + (primeX `shiftL` 1)
      y3 = y1 + (primeY `shiftL` 1)
   in recip (1.5 * 1.5)
        * cubicInterp
          ( cubicInterp
              (valCoord2 seed x0 y0)
              (valCoord2 seed x1 y0)
              (valCoord2 seed x2 y0)
              (valCoord2 seed x3 y0)
              xs
          )
          ( cubicInterp
              (valCoord2 seed x0 y1)
              (valCoord2 seed x1 y1)
              (valCoord2 seed x2 y1)
              (valCoord2 seed x3 y1)
              xs
          )
          ( cubicInterp
              (valCoord2 seed x0 y2)
              (valCoord2 seed x1 y2)
              (valCoord2 seed x2 y2)
              (valCoord2 seed x3 y2)
              xs
          )
          ( cubicInterp
              (valCoord2 seed x0 y3)
              (valCoord2 seed x1 y3)
              (valCoord2 seed x2 y3)
              (valCoord2 seed x3 y3)
              xs
          )
          ys
{-# INLINE [2] noise2Base #-}

noise3 :: (RealFrac a) => Noise3 a
noise3 = Noise3 noise3Base
{-# INLINE noise3 #-}

noise3Base :: (RealFrac a) => Seed -> a -> a -> a -> a
noise3Base seed x y z =
  let x1f = floor x
      y1f = floor y
      z1f = floor z

      xs = x - fromIntegral x1f
      ys = y - fromIntegral y1f
      zs = z - fromIntegral z1f

      x1 = x1f * primeX
      y1 = y1f * primeY
      z1 = z1f * primeZ
      x0 = x1 - primeX
      y0 = y1 - primeY
      z0 = z1 - primeZ
      x2 = x1 + primeX
      y2 = y1 + primeY
      z2 = z1 + primeZ
      x3 = x1 + (primeX `shiftL` 1)
      y3 = y1 + (primeY `shiftL` 1)
      z3 = z1 + (primeZ `shiftL` 1)
   in recip (1.5 * 1.5 * 1.5)
        * cubicInterp
          ( cubicInterp
              ( cubicInterp
                  (valCoord3 seed x0 y0 z0)
                  (valCoord3 seed x1 y0 z0)
                  (valCoord3 seed x2 y0 z0)
                  (valCoord3 seed x3 y0 z0)
                  xs
              )
              ( cubicInterp
                  (valCoord3 seed x0 y1 z0)
                  (valCoord3 seed x1 y1 z0)
                  (valCoord3 seed x2 y1 z0)
                  (valCoord3 seed x3 y1 z0)
                  xs
              )
              ( cubicInterp
                  (valCoord3 seed x0 y2 z0)
                  (valCoord3 seed x1 y2 z0)
                  (valCoord3 seed x2 y2 z0)
                  (valCoord3 seed x3 y2 z0)
                  xs
              )
              ( cubicInterp
                  (valCoord3 seed x0 y3 z0)
                  (valCoord3 seed x1 y3 z0)
                  (valCoord3 seed x2 y3 z0)
                  (valCoord3 seed x3 y3 z0)
                  xs
              )
              ys
          )
          ( cubicInterp
              ( cubicInterp
                  (valCoord3 seed x0 y0 z1)
                  (valCoord3 seed x1 y0 z1)
                  (valCoord3 seed x2 y0 z1)
                  (valCoord3 seed x3 y0 z1)
                  xs
              )
              ( cubicInterp
                  (valCoord3 seed x0 y1 z1)
                  (valCoord3 seed x1 y1 z1)
                  (valCoord3 seed x2 y1 z1)
                  (valCoord3 seed x3 y1 z1)
                  xs
              )
              ( cubicInterp
                  (valCoord3 seed x0 y2 z1)
                  (valCoord3 seed x1 y2 z1)
                  (valCoord3 seed x2 y2 z1)
                  (valCoord3 seed x3 y2 z1)
                  xs
              )
              ( cubicInterp
                  (valCoord3 seed x0 y3 z1)
                  (valCoord3 seed x1 y3 z1)
                  (valCoord3 seed x2 y3 z1)
                  (valCoord3 seed x3 y3 z1)
                  xs
              )
              ys
          )
          ( cubicInterp
              ( cubicInterp
                  (valCoord3 seed x0 y0 z2)
                  (valCoord3 seed x1 y0 z2)
                  (valCoord3 seed x2 y0 z2)
                  (valCoord3 seed x3 y0 z2)
                  xs
              )
              ( cubicInterp
                  (valCoord3 seed x0 y1 z2)
                  (valCoord3 seed x1 y1 z2)
                  (valCoord3 seed x2 y1 z2)
                  (valCoord3 seed x3 y1 z2)
                  xs
              )
              ( cubicInterp
                  (valCoord3 seed x0 y2 z2)
                  (valCoord3 seed x1 y2 z2)
                  (valCoord3 seed x2 y2 z2)
                  (valCoord3 seed x3 y2 z2)
                  xs
              )
              ( cubicInterp
                  (valCoord3 seed x0 y3 z2)
                  (valCoord3 seed x1 y3 z2)
                  (valCoord3 seed x2 y3 z2)
                  (valCoord3 seed x3 y3 z2)
                  xs
              )
              ys
          )
          ( cubicInterp
              ( cubicInterp
                  (valCoord3 seed x0 y0 z3)
                  (valCoord3 seed x1 y0 z3)
                  (valCoord3 seed x2 y0 z3)
                  (valCoord3 seed x3 y0 z3)
                  xs
              )
              ( cubicInterp
                  (valCoord3 seed x0 y1 z3)
                  (valCoord3 seed x1 y1 z3)
                  (valCoord3 seed x2 y1 z3)
                  (valCoord3 seed x3 y1 z3)
                  xs
              )
              ( cubicInterp
                  (valCoord3 seed x0 y2 z3)
                  (valCoord3 seed x1 y2 z3)
                  (valCoord3 seed x2 y2 z3)
                  (valCoord3 seed x3 y2 z3)
                  xs
              )
              ( cubicInterp
                  (valCoord3 seed x0 y3 z3)
                  (valCoord3 seed x1 y3 z3)
                  (valCoord3 seed x2 y3 z3)
                  (valCoord3 seed x3 y3 z3)
                  xs
              )
              ys
          )
          zs
{-# INLINEABLE [1] noise3Base #-}
