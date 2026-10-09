# pure-noise

Performant, modern noise generation for Haskell with a minimal dependency footprint.

## Core features

- **algebraic composition** of noise functions. You can combine,
  layer, and transform noise sources using standard operators (E.g., `Num`,
  `Fractional`, `Monad`, etc).
- **Complex effects** like domain warping and multi-octave fractals with clean,
  type-safe composition.
- **Competitive with C++**: roughly 70-100% of FastNoiseLite's single-threaded
  random-access throughput under the LLVM backend. And 2D cellular noise runs
  ahead of C++, measured against an optimized FNL build.

**For detailed FastNoiseLite comparison, methodology, and reproducibility instructions,
see the [benchmark README](https://github.com/jtnuttall/pure-noise/blob/main/bench/README.md).**

The public interface for this library is unlikely to change much, although the
implementations (`noiseBaseN` functions and anything in `Numeric.Noise.Internal`)
are subject to change and may change between minor versions.

## Important note on GHC 9.14

GHC 9.14 includes a substantial rewrite of the specializer. With this came some
regressions. Many of the ones I'm aware of are centered around newtype classes
(i.e., classes with one function), but aren't totally isolated to them.

As a result, using GHC 9.14 may **significantly alter your performance profile**.
This may be a regression.  **I have not yet had the time to test GHC 9.14
against this library's performance claims**, although CI tests that the library
compiles at all against 9.14.

If you run into issues on 9.14, opening an issue on this project's repository
would help a great deal: <https://github.com/jtnuttall/pure-noise/issues>

## Acknowledgments

- This project grew from a port of the excellent
  [FastNoiseLite](https://github.com/Auburn/FastNoiseLite) library. The library
  structure has been tuned to perform well in Haskell and fit well with Haskell
  semantics, but the core noise share their origin with FNL.
- All credit for the original design, algorithms, and implementation goes to its
  creator **[Jordan Peck (@Auburn)](https://github.com/Auburn)**. I'm grateful for
  their work and the opportunity to learn from it.
- The original FastNoiseLite code, from which the core algorithms in this library
  were originally ported, is (C) 2020 Jordan Peck and is licensed under the MIT
  license, a copy of which is included in this repository.

## FastNoiseLite compatibility

pure-noise began as a port of FastNoiseLite and shares its algorithmic
lineage, but it isn't a 1:1 port. Version `0.3` targets FNL equivalence within a
few ULP.

### Notable differences to FNL

#### Fractals

- `pingPong` uses an algebraically equivalent equation for FNL's triangle wave,
  but differs by a few ULP.
- `billow` has no FNL equivalent.

> [!IMPORTANT]
>
> 0.3 changes the seed-to-output mapping of the `value`, `valueCubic`, and
> fractal families relative to 0.2.x. Pin `pure-noise < 0.3` if you depend on
> stable output from those functions.

## Usage

The library provides composable noise functions. `Noise2` and `Noise3` are type
aliases for 2D and 3D noise. Noise functions can be composed transparently using
standard operators with minimal performance cost.

Most noise functions produce values in `[-1, 1]`, give or take small
floating-point excursions. The exception is cellular noise with the
`DistManhattan` or `DistHybrid` distance functions, whose distances are
unnormalized and can exceed 1 (up to ~2 in practice).

### Basic Example

```haskell
import Numeric.Noise qualified as Noise

-- Compose multiple noise sources
myNoise2 :: (RealFrac a) => Noise.Seed -> a -> a -> a
myNoise2 =
  let fractalConfig = Noise.defaultFractalConfig
      combined = (Noise.perlin2 + Noise.smootherSimplex2) / 2
  in Noise.noise2At $ Noise.fractal2 fractalConfig combined
```

### Advanced Features

The library's unified `Noise p v` type enables powerful composition patterns:

#### Complex Compositions

The `Monad` instance is useful to create noise that depends on other noise values:

```haskell
-- Use one noise function's output to modulate another
complexNoise :: Noise.Noise2 Float
complexNoise = do
  baseNoise <- Noise.perlin2
  detailNoise <- Noise.next2 Noise.smootherSimplex2
  -- Blend based on base noise: smooth areas get less detail
  pure $ baseNoise * 0.7 + detailNoise * (0.3 * (1 + baseNoise) / 2)
```

This is especially useful for creating organic, varied terrain where one noise pattern
influences the characteristics of another.

#### 1D Noise via Slicing

Generate 1D noise by slicing higher-dimensional noise at a fixed coordinate:

```haskell
-- Create 1D noise by fixing one dimension
noise1d :: Noise.Noise1 Float
noise1d = Noise.sliceY2 0.0 Noise.perlin2

-- Evaluate at a point
value = Noise.noise1At noise1d seed 5.0
```

**Coordinate Transformation:**

Scale, rotate, or warp the coordinate space:

```haskell
-- Double the frequency
scaled = Noise.warp (\(x, y) -> (x * 2, y * 2)) Noise.perlin2

-- Rotate 45 degrees
rotated = Noise.warp (\(x, y) ->
  let a = pi / 4
  in (x * cos a - y * sin a, x * sin a + y * cos a)) Noise.perlin2
```

#### Layering Independent Noise

Use `reseed` or `next2`/`next3` to create independent layers:

```haskell
layered = (Noise.perlin2 + Noise.next2 Noise.perlin2) / 2
```

More examples can be found in `bench` and `demo`.

#### Domain Warping

Domain warping uses one noise function to distort the coordinate space of another,
creating organic, flowing patterns ideal for terrain, clouds, and natural textures:

```haskell
domainWarped :: Noise.Noise2 Float
domainWarped = do
  -- Generate 3D fractal for warp offsets
  let warpNoise = Noise.fractal3 Noise.defaultFractalConfig{Noise.octaves = 5} Noise.perlin3
  -- Sample 3D noise at different slices to create warp offsets
  warpX <- Noise.sliceX3 0.0 warpNoise  -- Samples at (0, x, y)
  warpY <- Noise.sliceY3 0.0 warpNoise  -- Samples at (x, 0, y)
  -- Apply warping to base noise coordinates
  Noise.warp (\(x, y) -> (x + 30 * warpX, y + 30 * warpY))
    $ Noise.fractal2 Noise.defaultFractalConfig{Noise.octaves = 5} Noise.openSimplex2
```

![Domain Warped Noise](https://raw.githubusercontent.com/jtnuttall/pure-noise/main/demo/images/domain-warp.png)

See the [demo app](demo/) for an interactive version with adjustable parameters.

## Performance notes

- In single-threaded scenarios with LLVM enabled, this library reaches
  **roughly 70-100% of C++ FastNoiseLite throughput at random access, with 2D
  cellular noise ~7% faster than C++**. Grid-coherent workloads measure lower
  for the simplex family. These numbers are measured against an optimized
  FNL build (AVX2+FMA, FP contraction on).
- This library performs significantly better under the LLVM backend
  (`-fllvm`); the native code generator is not recommended where generation
  speed is a real concern.
- See the [benchmark README](https://github.com/jtnuttall/pure-noise/blob/main/bench/README.md)
  for per-algorithm, per-workload results and methodology.

### Recommended build flags

#### Native optimization flags for the library

For the library itself, copy `cabal.project.local.template` into your project
(`+optimize` is on by default; `+mfma +mavx` are safe on modern x86-64).

For bulk generation, prefer parallel evaluation via `massiv`.

#### Fused multiply add

For the fastest downstream executables, compile the modules that _call_ the
noise functions (the kernels inline into your code) with:

```
-fllvm -mavx -mfma -optlc-fp-contract=fast
```

> [!WARNING]
>
> 1. Results differ from a build without fma fusion by a few ULP.
> 2. Fusion decisions may vary across LLVM versions.
>    Skip this flag if you need bit-identical output across builds/toolchains.
> 3. You must use `-fllvm` to use this flag. It has no effect on the native
>    code generator.

`-optlc-fp-contract=fast` lets LLVM fuse multiply-add chains into FMA
instructions, an optimization modern C++ compilers apply by default. This
gives a ~5-15% improvement on these kernels in testing.

### Parallel noise generation

This library integrates well with [massiv](https://hackage.haskell.org/package/massiv)
for parallel computation. Parallel evaluation reaches roughly 6-9x
single-threaded throughput on a 14-core machine in pinned-clock measurements.

> [!IMPORTANT]
>
> Massiv integration is the recommended approach for generating large noise
> textures or datasets.

### Benchmarks

#### Results

Measured by values / second generated by the noise functions, in the
`llvm-bench` configuration (LLVM backend + FP contraction), on an i7-1370P
**pinned at 1.9 GHz** for measurement stability —

Absolute figures scale with CPU clock. The FastNoiseLite ratios are intended
to be clock-invariant.

There's inevitably some noise in the measurements because the results are forced
into an unboxed vector.

> [!NOTE]
>
> These numbers are lower than the initial release because they were re-run on a
> slower processor. Order-of-magnitude/comparative difference remains reasonably
> stable. See the benchmark README for details.

##### 2D

| name             | Float (values/sec) | Double (values/sec) |
| ---------------- | ------------------ | ------------------- |
| value2           | 64_407_126         | 68_260_653          |
| perlin2          | 61_301_663         | 65_143_707          |
| openSimplex2     | 25_982_291         | 27_045_472          |
| valueCubic2      | 22_743_642         | 23_403_842          |
| smootherSimplex2 | 17_167_069         | 17_762_669          |
| cellular2        | 16_025_950         | 16_007_044          |

##### 3D

| name             | Float (values/sec) | Double (values/sec) |
| ---------------- | ------------------ | ------------------- |
| value3           | 34_673_623         | 35_929_146          |
| perlin3          | 29_325_590         | 30_432_482          |
| openSimplex3     | 10_975_857         | 10_922_644          |
| smootherSimplex3 | 9_232_128          | 9_166_843           |
| valueCubic3      | 7_453_365          | 7_278_612           |
| cellular3        | 5_238_497          | 5_061_911           |

## Examples

There's an interactive [demo app](https://github.com/jtnuttall/pure-noise/tree/main/demo) in the `demo` directory.

### OpenSimplex2

![OpenSimplex2](https://raw.githubusercontent.com/jtnuttall/pure-noise/main/demo/images/opensimplex.png)
![OpenSimplex2 ridged](https://raw.githubusercontent.com/jtnuttall/pure-noise/main/demo/images/opensimplex-ridged.png)

### Perlin

![Perlin fBm](https://raw.githubusercontent.com/jtnuttall/pure-noise/main/demo/images/perlin-fbm.png)

### Cellular

![value](https://raw.githubusercontent.com/jtnuttall/pure-noise/main/demo/images/cell-value.png)
![distance2add](https://raw.githubusercontent.com/jtnuttall/pure-noise/main/demo/images/cell-d2.png)
