module Noise2Spec where

import Numeric.Noise

seed :: Seed
seed = 55

prop_addition :: Rational -> Rational -> Bool
prop_addition x y = noise2At (const2 x + const2 y) seed x y == x + y

prop_addition_associative :: Rational -> Rational -> Bool
prop_addition_associative x y =
  let n1 = const2 2003
      n2 = const2 2027
      n3 = const2 2069
   in noise2At ((n1 + n2) + n3) seed x y == noise2At (n1 + (n2 + n3)) seed x y

prop_addition_commutative :: Rational -> Rational -> Bool
prop_addition_commutative x y =
  let n1 = const2 x
      n2 = const2 y
   in noise2At (n1 + n2) seed x y == noise2At (n2 + n1) seed x y

prop_0_is_additive_identity :: Rational -> Rational -> Rational -> Bool
prop_0_is_additive_identity v x y =
  let n1 = const2 v
   in noise2At (n1 + 0) seed x y == noise2At n1 seed x y

prop_negate_is_additive_inverse :: Rational -> Rational -> Rational -> Bool
prop_negate_is_additive_inverse v x y =
  let n1 = const2 v
   in noise2At (n1 - n1) seed x y == 0

prop_multiplication :: Rational -> Rational -> Bool
prop_multiplication x y = noise2At (const2 x * const2 y) seed x y == x * y

prop_multiplication_associative :: Rational -> Rational -> Bool
prop_multiplication_associative x y =
  let n1 = const2 2003
      n2 = const2 2027
      n3 = const2 2069
   in noise2At ((n1 * n2) * n3) seed x y == noise2At (n1 * (n2 * n3)) seed x y
