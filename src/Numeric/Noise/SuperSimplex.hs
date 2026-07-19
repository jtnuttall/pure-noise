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

  -- * 3D Noise
  noise3,
  noise3Base,
) where

import Data.Bits
import Data.Bool (bool)
import Numeric.Noise.Internal
import Numeric.Noise.Internal.Math

noise2 :: (RealFrac a) => Noise2 a
noise2 = mkNoise2 noise2Base
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
{-# INLINE [2] noise2Base #-}

attenuate :: (RealFrac a) => a -> Seed -> Hash -> Hash -> a -> a -> a
attenuate !vi !seed !i !j !x !y =
  let !v = max 0 vi
   in (v * v) * (v * v) * gradCoord2 seed i j x y
{-# INLINE attenuate #-}

normalize :: (RealFrac a) => a -> a
normalize = (18.24196194486065 *)
{-# INLINE normalize #-}

noise3 :: (RealFrac a) => Noise3 a
noise3 = mkNoise3 noise3Base
{-# INLINE noise3 #-}

-- | 3D OpenSimplex2S, ported from FNL's @SingleOpenSimplex2S@ (two offset
-- rotated cube grids), including the mandatory rotation FNL applies in
-- @TransformNoiseCoordinate3D@. Keep the expression shapes and the skip-flag
-- control flow: FP reassociation changes outputs (pinned by the golden
-- tests). Close to FNL but not bit-exact; scripts/fnl-diff measures the ulp
-- envelope.
noise3Base :: (RealFrac a) => Seed -> a -> a -> a -> a
noise3Base seed xo yo zo =
  let (x, y, z) = rotate3 xo yo zo

      fi = floor x :: Hash
      fj = floor y :: Hash
      fk = floor z :: Hash
      xi = x - fromIntegral fi
      yi = y - fromIntegral fj
      zi = z - fromIntegral fk

      i = fi * primeX
      j = fj * primeY
      k = fk * primeZ
      seed2 = seed + 1293373

      -- FNL: (int)(-0.5f - xi), i.e. -1 when the offset is >= 0.5, else 0
      xnm = bool 0 (-1) (xi >= 0.5) :: Hash
      ynm = bool 0 (-1) (yi >= 0.5) :: Hash
      znm = bool 0 (-1) (zi >= 0.5) :: Hash

      x0 = xi + fromIntegral xnm
      y0 = yi + fromIntegral ynm
      z0 = zi + fromIntegral znm
      a0 = 0.75 - x0 * x0 - y0 * y0 - z0 * z0
      v0 =
        q a0
          * gradCoord3 seed (i + (xnm .&. primeX)) (j + (ynm .&. primeY)) (k + (znm .&. primeZ)) x0 y0 z0

      x1 = xi - 0.5
      y1 = yi - 0.5
      z1 = zi - 0.5
      a1 = 0.75 - x1 * x1 - y1 * y1 - z1 * z1
      v1 = q a1 * gradCoord3 seed2 (i + primeX) (j + primeY) (k + primeZ) x1 y1 z1

      xFlip0 = fromIntegral ((xnm .|. 1) `shiftL` 1) * x1
      yFlip0 = fromIntegral ((ynm .|. 1) `shiftL` 1) * y1
      zFlip0 = fromIntegral ((znm .|. 1) `shiftL` 1) * z1
      xFlip1 = fromIntegral (-2 - (xnm `shiftL` 2)) * x1 - 1.0
      yFlip1 = fromIntegral (-2 - (ynm `shiftL` 2)) * y1 - 1.0
      zFlip1 = fromIntegral (-2 - (znm `shiftL` 2)) * z1 - 1.0

      a2 = xFlip0 + a0
      ~(vX, skip5)
        | a2 > 0 =
            let ~x2 = x0 - fromIntegral (xnm .|. 1)
             in ( q a2
                    * gradCoord3 seed (i + (complement xnm .&. primeX)) (j + (ynm .&. primeY)) (k + (znm .&. primeZ)) x2 y0 z0
                , False
                )
        | otherwise =
            let a3 = yFlip0 + zFlip0 + a0
                ~v3
                  | a3 > 0 =
                      let ~y3 = y0 - fromIntegral (ynm .|. 1)
                          ~z3 = z0 - fromIntegral (znm .|. 1)
                       in q a3
                            * gradCoord3 seed (i + (xnm .&. primeX)) (j + (complement ynm .&. primeY)) (k + (complement znm .&. primeZ)) x0 y3 z3
                  | otherwise = 0
                a4 = xFlip1 + a1
                ~(v4, sk)
                  | a4 > 0 =
                      let ~x4 = fromIntegral (xnm .|. 1) + x1
                       in ( q a4
                              * gradCoord3 seed2 (i + (xnm .&. (primeX * 2))) (j + primeY) (k + primeZ) x4 y1 z1
                          , True
                          )
                  | otherwise = (0, False)
             in (v3 + v4, sk)

      a6 = yFlip0 + a0
      ~(vY, skip9)
        | a6 > 0 =
            let ~y6 = y0 - fromIntegral (ynm .|. 1)
             in ( q a6
                    * gradCoord3 seed (i + (xnm .&. primeX)) (j + (complement ynm .&. primeY)) (k + (znm .&. primeZ)) x0 y6 z0
                , False
                )
        | otherwise =
            let a7 = xFlip0 + zFlip0 + a0
                ~v7
                  | a7 > 0 =
                      let ~x7 = x0 - fromIntegral (xnm .|. 1)
                          ~z7 = z0 - fromIntegral (znm .|. 1)
                       in q a7
                            * gradCoord3 seed (i + (complement xnm .&. primeX)) (j + (ynm .&. primeY)) (k + (complement znm .&. primeZ)) x7 y0 z7
                  | otherwise = 0
                a8 = yFlip1 + a1
                ~(v8, sk)
                  | a8 > 0 =
                      let ~y8 = fromIntegral (ynm .|. 1) + y1
                       in ( q a8
                              * gradCoord3 seed2 (i + primeX) (j + (ynm .&. (primeY `shiftL` 1))) (k + primeZ) x1 y8 z1
                          , True
                          )
                  | otherwise = (0, False)
             in (v7 + v8, sk)

      aA = zFlip0 + a0
      ~(vZ, skipD)
        | aA > 0 =
            let ~zA = z0 - fromIntegral (znm .|. 1)
             in ( q aA
                    * gradCoord3 seed (i + (xnm .&. primeX)) (j + (ynm .&. primeY)) (k + (complement znm .&. primeZ)) x0 y0 zA
                , False
                )
        | otherwise =
            let aB = xFlip0 + yFlip0 + a0
                ~vB
                  | aB > 0 =
                      let ~xB = x0 - fromIntegral (xnm .|. 1)
                          ~yB = y0 - fromIntegral (ynm .|. 1)
                       in q aB
                            * gradCoord3 seed (i + (complement xnm .&. primeX)) (j + (complement ynm .&. primeY)) (k + (znm .&. primeZ)) xB yB z0
                  | otherwise = 0
                aC = zFlip1 + a1
                ~(vC, sk)
                  | aC > 0 =
                      let ~zC = fromIntegral (znm .|. 1) + z1
                       in ( q aC
                              * gradCoord3 seed2 (i + primeX) (j + primeY) (k + (znm .&. (primeZ `shiftL` 1))) x1 y1 zC
                          , True
                          )
                  | otherwise = (0, False)
             in (vB + vC, sk)

      ~v5
        | not skip5
        , a5 <- yFlip1 + zFlip1 + a1
        , a5 > 0 =
            let ~y5 = fromIntegral (ynm .|. 1) + y1
                ~z5 = fromIntegral (znm .|. 1) + z1
             in q a5
                  * gradCoord3 seed2 (i + primeX) (j + (ynm .&. (primeY `shiftL` 1))) (k + (znm .&. (primeZ `shiftL` 1))) x1 y5 z5
        | otherwise = 0

      ~v9
        | not skip9
        , a9 <- xFlip1 + zFlip1 + a1
        , a9 > 0 =
            let ~x9 = fromIntegral (xnm .|. 1) + x1
                ~z9 = fromIntegral (znm .|. 1) + z1
             in q a9
                  * gradCoord3 seed2 (i + (xnm .&. (primeX * 2))) (j + primeY) (k + (znm .&. (primeZ `shiftL` 1))) x9 y1 z9
        | otherwise = 0

      ~vD
        | not skipD
        , aD <- xFlip1 + yFlip1 + a1
        , aD > 0 =
            let ~xD = fromIntegral (xnm .|. 1) + x1
                ~yD = fromIntegral (ynm .|. 1) + y1
             in q aD
                  * gradCoord3 seed2 (i + (xnm .&. (primeX `shiftL` 1))) (j + (ynm .&. (primeY `shiftL` 1))) (k + primeZ) xD yD z1
        | otherwise = 0
   in (v0 + v1 + vX + vY + vZ + v5 + v9 + vD) * 9.046026385208288
 where
  q a = (a * a) * (a * a)
  {-# INLINE q #-}
{-# INLINE [2] noise3Base #-}
