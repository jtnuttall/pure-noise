# Changelog for `pure-noise`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- `doctest-parallel` now tests examples.

### Changed

- BREAKING - fractals are now configured using a step function that
  returns an unboxed tuple. This replaces the (potentially boxed) higher-order
  modifier function, and changes the vocabulary and performance characteristics
  of fractals somewhat.
  - **This fundamentally alters the appearance and value range of fractals.**
- BREAKING - renamed `superSimplex` to `smootherSimplex`. Algorithm is
  identical, but naming is clearer.
- BREAKING - value and cubic value noise, in both 2D and 3D variants, now
  implement the exact same coordinate function as FNL: It squares the hash and
  distributes the squared hash along the whole calculation.
  - **This fundamentally changes all values that these functions produce.**
- BREAKING - fractals with `octaves == 1` now return the noise unchanged. ALL
  fractals now produce one less iteration than before. This should prove less
  surprising to users coming from other noise libraries like FNL.
- `OpenSimplex2S` is the smoother variant of `OpenSimplex2` noise. Originally
  decided to omit the `2`/`2S` from OpenSimplex since it created collisions with
  the noise function naming scheme that seemed most ergonomic; I think this
  should neatly resolve remaining semantic ambiguity.
- Improved documentation for flags, fixed some rendering quirks.

## 0.2.2.0 2026-07-19

This is the final release of the 0.2.2.0 line.

### Added

- Exports for `mkNoise1`, `mkNoise2`, and `mkNoise3`, in case library users
  wish to hoist custom kernels.
- FastNoiseLite comparisons are now in-repository and reproducible. Comparison
  mechanism goes through tasty-bench.
- The sdist now includes `bench/fnl-compare/cbits/FastNoiseLite.h`, so the
  FNL comparison benchmark can build from a Hackage tarball.
- 3D counterparts for the simplex and cellular families: `openSimplex3`,
  `superSimplex3`, and `cellular3`.
- Flag `optimize`, with default `true`, which bumps the library's optimization level
  to `-O2` by default. Refer to the flag's documentation for rationale.
- Flag `mfma`, which will set `-mfma` for GHC, and may open pathways to
  fused-multiply-add optimizations
- Flag `mavx`, which will set `-mavx` for GHC, and may open pathways for AVX2
  SIMD optimization in the future.
- Flag `llvm-bench` (manual, default off) gating all LLVM-specific benchmark
  options (`-fllvm`, pinned `opt`/`llc`/`clang`, `-mavx`, `-mfma`,
  `-optlc-fp-contract=fast`).
- Benchmark runs now write a `.meta` provenance sidecar (toolchain versions,
  CPU governor/turbo state, AC power state, pinning) next to each CSV.

### Changed

- `pure-noise` now builds at `-O2` by default.
- Project now uses haskell.nix instead of Stack.
- Various updates to documentation, to improve quality
- Performance documentation and results tables re-collected on the 0.2.2.0
  toolchain (README.md and bench/README.md).
- `tested-with: GHC == 9.6.7, 9.8.4, 9.12.2`.

### Fixed

- Replace `round`-based rounding with a truncating `fastRound`. This replaces an
  `rintFloat` call in the core lowering with a `float2Int` call, which improves
  performance in hot loops.
  - This changes `cellular2` output at half-integer coordinates: cell selection
    now rounds half away from zero, matching FastNoiseLite's `FastRound`, rather
    than half to even. Numerically observable but visually imperceptible.
- Match FastNoiseLite's evaluation order in `openSimplex2`'s middle-corner
  contribution. Output changes by a few ULP.
- Fix an issue with the 3D benchmark harness where degenerate index math
  collapsed the sample set to ~4 distinct points, providing a poor signal on
  real-world performance.

## 0.2.1.1 2025-10-31

### Changed

- Fixed some errata in the documentation

## 0.2.1.0 2025-10-31

### Added

- 1D noise support with `noise1At` evaluation function
- Noise slicing functions (`sliceX2`, `sliceY2`, `sliceX3`, `sliceY3`, `sliceZ3`) for reducing noise dimensionality
- Comprehensive Haddock documentation for:
  - Core `Noise p v` type with usage examples
  - All utility functions (`warp`, `reseed`, `remap`, `blend`)
  - All slicing functions with examples
- Exported utility functions:
  - `warp` - transform coordinate space
  - `reseed` - modify seed for independent layers (generalizes `alterSeed2`/`alterSeed3`)
  - `remap` - transform noise values (alias for `fmap`)
  - `blend` - combine noise functions with custom blending (alias for `liftA2`)
- Exported `Noise` type for advanced usage and type annotations

### Changed

- Refactored to unified noise representation using `newtype Noise p v`
  - `Noise2` and `Noise3` are now type aliases: `Noise (a,a) a` and `Noise (a,a,a) a`
  - Provides `Functor`, `Applicative`, `Monad`, `Num`, `Fractional`, and `Floating` instances
  - All dimensions share the same instance implementations for consistency
- Updated module documentation with examples of new features (slicing, warping, layering)
- Improved README with advanced usage examples

### Performance

- **Overall**: 90% of benchmarks improved with +32.4% average performance gain
- **Ping-pong fractals**: Branchless optimization yields 110-148% improvement (perlin, value, openSimplex)
- **Cellular noise**: REWRITE RULES for gradient lookups provide 61-70% improvement
- **3D ValueCubic fractals**: Fixed performance regression, achieving 45-80% improvement
- **Perlin noise**: 10-63% improvements from optimized lerp/cubic interpolation with REWRITE RULES
- **Value noise**: 11-43% improvements from interpolation optimizations
- **OpenSimplex2**: 6-16% improvement for Float; minor regression (~5%) for Double variants
- **SuperSimplex2**: Shows 3-21% regression due to improved benchmark methodology
  - Previous benchmarks used same X/Y offset (diagonal sampling), which favored SuperSimplex's triangular lattice
  - New benchmarks use independent X/Y offsets for realistic 2D coordinate distributions
  - Algorithm remains functionally correct; numbers now reflect true 2D performance

## 0.2.0.0 - 2025-10-21

### Added

- Comprehensive haddock documentation for main `Numeric.Noise` module with usage examples

### Changed

- Migrated internal implementation from `vector` to `primitive` (PrimArray)
- Removed `vector` dependency from library (still used in benchmarks)
- Require GHC 9.2+ (base >= 4.16)
- Hide internal modules from public API (`Numeric.Noise.Internal`, `Numeric.Noise.Internal.Math`)
- Improved cellular noise performance by 20-30% through specialized computation paths for different result types

### Fixed

- Fixed intermediate list allocation in fractal functions on GHC 9.6+
- Improved division performance for `Noise3` instances

## 0.1.0.1 - 2024-10-15

- Add bounds for vector
- Add `next` combinator

## 0.1.0.0 - 2024-10-15

- Initial release
