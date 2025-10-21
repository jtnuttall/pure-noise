-- |
-- Maintainer: Jeremy Nuttall <jeremy@jeremy-nuttall.com>
-- Stability : experimental
module Numeric.Noise.Perlin (
  -- * 2D
  noise2,
  noise2Base,

  -- * 3D
  noise3,
  noise3Base,
)
where

import Numeric.Noise.Internal
import Numeric.Noise.Internal.Math

noise2 :: (RealFrac a) => Noise2 a
noise2 = Noise2 noise2Base
{-# INLINE noise2 #-}

noise2Base :: forall a. (RealFrac a) => Seed -> a -> a -> a
noise2Base seed x y =
  let x0 = floor x
      y0 = floor y

      xd0 = x - fromIntegral x0
      yd0 = y - fromIntegral y0
      xd1 = xd0 - 1
      yd1 = yd0 - 1

      u = quinticInterp xd0
      v = quinticInterp yd0

      x0p = x0 * primeX
      y0p = y0 * primeY

      x1p = x0p + primeX
      y1p = y0p + primeY
   in 1.4247691104677813
        * lerp
          ( lerp
              (gradCoord2 seed x0p y0p xd0 yd0)
              (gradCoord2 seed x1p y0p xd1 yd0)
              u
          )
          ( lerp
              (gradCoord2 seed x0p y1p xd0 yd1)
              (gradCoord2 seed x1p y1p xd1 yd1)
              u
          )
          v
{-# INLINE noise2Base #-}

noise3 :: (RealFrac a) => Noise3 a
noise3 = Noise3 noise3Base
{-# INLINE noise3 #-}

noise3Base :: (RealFrac a) => Seed -> a -> a -> a -> a
noise3Base seed x y z =
  let x0 = floor x
      y0 = floor y
      z0 = floor z

      xd0 = x - fromIntegral x0
      yd0 = y - fromIntegral y0
      zd0 = z - fromIntegral z0

      xd1 = xd0 - 1
      yd1 = yd0 - 1
      zd1 = zd0 - 1

      u = quinticInterp xd0
      v = quinticInterp yd0
      w = quinticInterp zd0

      x0p = x0 * primeX
      y0p = y0 * primeY
      z0p = z0 * primeZ
      x1p = x0p + primeX
      y1p = y0p + primeY
      z1p = z0p + primeZ
   in 0.96492141485214233398437
        * lerp
          ( lerp
              ( lerp
                  (gradCoord3 seed x0p y0p z0p xd0 yd0 zd0)
                  (gradCoord3 seed x1p y0p z0p xd1 yd0 zd0)
                  u
              )
              ( lerp
                  (gradCoord3 seed x0p y1p z0p xd0 yd1 zd0)
                  (gradCoord3 seed x1p y1p z0p xd1 yd1 zd0)
                  u
              )
              v
          )
          ( lerp
              ( lerp
                  (gradCoord3 seed x0p y0p z1p xd0 yd0 zd1)
                  (gradCoord3 seed x1p y0p z1p xd1 yd0 zd1)
                  u
              )
              ( lerp
                  (gradCoord3 seed x0p y1p z1p xd0 yd1 zd1)
                  (gradCoord3 seed x1p y1p z1p xd1 yd1 zd1)
                  u
              )
              v
          )
          w
{-# INLINE noise3Base #-}
