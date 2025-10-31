module Noise3Spec where

import Numeric.Noise

seed :: Seed
seed = 2381

prop_addition :: Rational -> Rational -> Rational -> Bool
prop_addition x y z =
  noise3At (const3 x + const3 y + const3 z) seed x y z == x + y + z

prop_addition_associative :: Rational -> Rational -> Rational -> Bool
prop_addition_associative x y z =
  let n1 = const3 2003
      n2 = const3 2027
      n3 = const3 2069
   in noise3At ((n1 + n2) + n3) seed x y z == noise3At (n1 + (n2 + n3)) seed x y z

prop_addition_commutative :: Rational -> Rational -> Rational -> Bool
prop_addition_commutative x y z =
  let n1 = const3 x
      n2 = const3 y
   in noise3At (n1 + n2) seed x y z == noise3At (n2 + n1) seed x y z

prop_0_is_additive_identity :: Rational -> Rational -> Rational -> Rational -> Bool
prop_0_is_additive_identity v x y z =
  let n1 = const3 v
   in noise3At (n1 + 0) seed x y z == noise3At n1 seed x y z

prop_negate_is_additive_inverse :: Rational -> Rational -> Rational -> Rational -> Bool
prop_negate_is_additive_inverse v x y z =
  let n1 = const3 v
   in noise3At (n1 - n1) seed x y z == 0

prop_multiplication :: Rational -> Rational -> Rational -> Bool
prop_multiplication x y z =
  noise3At (const3 x * const3 y * const3 z) seed x y z == x * y * z

prop_multiplication_associative :: Rational -> Rational -> Rational -> Bool
prop_multiplication_associative x y z =
  let n1 = const3 2003
      n2 = const3 2027
      n3 = const3 2069
   in noise3At ((n1 * n2) * n3) seed x y z == noise3At (n1 * (n2 * n3)) seed x y z

prop_1_is_multiplicative_identity :: Rational -> Rational -> Rational -> Rational -> Bool
prop_1_is_multiplicative_identity v x y z =
  let n1 = const3 v
   in noise3At (n1 * 1) seed x y z == v
