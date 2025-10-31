# Changelog for `pure-noise`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

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
