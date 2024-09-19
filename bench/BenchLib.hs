module BenchLib (
  Benchmark,
  Benchmarkable,
  bgroup,
  defaultMain,
  env,
  bench,
  nf,
  whnf,
) where

import Test.Tasty.Bench

-- import Criterion
-- import Criterion.Main