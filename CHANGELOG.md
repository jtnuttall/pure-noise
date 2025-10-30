# Changelog for `pure-noise`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

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

- Branchless OpenSimplex optimization (8-25% improvement depending on variant)
- Branchless ping-pong fractal optimization (~45% improvement)
- Added RULES pragmas for gradient lookups (~6-9% global improvement)
- Fixed fractal performance regression

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
