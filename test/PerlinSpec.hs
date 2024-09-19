module PerlinSpec where

import Numeric.Noise

seed :: Seed
seed = 82384

prop_noise2_normalized :: Double -> Double -> Bool
prop_noise2_normalized x y =
  let n = noise2At perlin2 seed x y
   in n >= -1 && n <= 1

prop_noise2_addition_associative :: Rational -> Rational -> Bool
prop_noise2_addition_associative x y =
  noise2At ((perlin2 + perlin2) + perlin2) seed x y == noise2At (perlin2 + (perlin2 + perlin2)) seed x y

prop_noise2_addition_commutative :: Rational -> Rational -> Bool
prop_noise2_addition_commutative x y =
  noise2At (perlin2 + perlin2) seed x y == noise2At (perlin2 + perlin2) seed x y