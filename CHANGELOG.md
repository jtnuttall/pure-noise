# Changelog for `pure-noise`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

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
