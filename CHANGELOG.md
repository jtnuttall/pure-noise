# Changelog for `pure-noise`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- 1D noise support with `noise1At` evaluation function
- Noise slicing functions (`sliceX`, `sliceY`, `sliceZ`) and `SliceNoise` typeclass for reducing noise dimensionality
- `NoiseN` typeclass with dimension-polymorphic methods:
  - `constNoise` - create constant noise field
  - `mapSeed` - alter the seed for independent noise layers (generalizes `alterSeed2`/`alterSeed3`)
  - `mapNoise` - apply transformations to noise output (generalizes `map2`/`map3`)
  - `combineNoise` - combine two noise functions pointwise
  - `clampNoise` - clamp noise output between bounds
- Exported `Noise` data family for direct access to noise constructors

### Changed

- Refactored to type-indexed noise representation using `Noise (dim :: Nat) a` pattern
  - `Noise2` and `Noise3` remain as type aliases for backward compatibility
  - Dimension-specific functions (`alterSeed2`, `alterSeed3`, `map2`, `map3`, `const2`, `const3`) are now superseded by polymorphic `NoiseN` methods

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
