# pure-noise

Performant, modern noise generation for Haskell with a minimal dependency footprint.

## Core features

- **algebraic composition** of noise functions. You can combine,
  layer, and transform noise sources using standard operators (E.g., `Num`,
  `Fractional`, `Monad`, etc).
- **Complex effects** like domain warping and multi-octave fractals with clean,
  type-safe composition.
- **84-95% of C++ FastNoiseLite performance** through aggressive optimization and
  LLVM compilation ([see benchmarks](bench/README.md)).

**For detailed FastNoiseLite comparison, methodology, and reproducibility instructions,
see the [benchmark README](bench/README.md).**

The public interface for this library is unlikely to change much, although the
implementations (`noiseBaseN` functions and anything in `Numeric.Noise.Internal`)
are subject to change and may change between minor versions.

## Credit

- The algorithms used in this library are ported from
  [FastNoiseLite](https://github.com/Auburn/FastNoiseLite). The library structure
  has been retuned to fit better with Haskell semantics.
- **All credit goes to [@Auburn](https://github.com/Auburn) for the terrific FastNoiseLite implementation and
  the opportunity to learn from it.**

## Usage

The library provides composable noise functions `Noise2` and `Noise3` are type
aliases for 2D and 3D noise. Noise functions can be composed transparently using
standard operators with minimal performance cost.

Noise values are generally clamped to `[-1, 1]`, although some noise functions
may occasionally produce values slightly outside this range.

### Basic Example

```haskell
import Numeric.Noise qualified as Noise

-- Compose multiple noise sources
myNoise2 :: (RealFrac a) => Noise.Seed -> a -> a -> a
myNoise2 =
  let fractalConfig = Noise.defaultFractalConfig
      combined = (Noise.perlin2 + Noise.superSimplex2) / 2
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
  detailNoise <- Noise.next2 Noise.superSimplex2
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
  -- Extract X and Y warp offsets by slicing at z=0
  warpX <- Noise.sliceX3 0.0 warpNoise
  warpY <- Noise.sliceY3 0.0 warpNoise
  -- Apply warping to base noise coordinates
  Noise.warp (\(x, y) -> (x + 30 * warpX, y + 30 * warpY))
    $ Noise.fractal2 Noise.defaultFractalConfig{Noise.octaves = 5} Noise.openSimplex2
```

![Domain Warped Noise](https://raw.githubusercontent.com/jtnuttall/pure-noise/main/demo/images/domain-warp.png)

See the [demo app](demo/) for an interactive version with adjustable parameters.

## Performance notes

- In single-threaded scenarios with LLVM enabled, this library achieves **84-95%
  of C++ FastNoiseLite performance**.
- This library benefits considerably from compilation with the LLVM backend
  (`-fllvm`). Benchmarks suggest a ~50-80% difference depending on the kind of noise.

### Parallel noise generation

This library integrates well with [massiv](https://hackage.haskell.org/package/massiv)
for parallel computation. Parallel performance can reach 10-15× single-threaded
performance.

**This is the recommended approach for generating large noise textures or datasets.**

### Benchmarks

#### Results

Measured by values / second generated by the noise functions. These results come
from a benchmark with `-fllvm` enabled.

There's inevitably some noise in the measurements because all of the results are
forced into an unboxed vector.

##### 2D

| name          | Float (values/sec) | Double (values/sec) |
| ------------- | ------------------ | ------------------- |
| value2        | 173_511_654        | 189_119_731         |
| perlin2       | 154_674_464        | 161_114_532         |
| openSimplex2  | 74_747_031         | 74_332_345          |
| valueCubic2   | 61_415_544         | 62_481_313          |
| superSimplex2 | 51_295_369         | 50_383_577          |
| cellular2     | 34_996_382         | 32_652_899          |

##### 3D

| name        | Float (values/sec) | Double (values/sec) |
| ----------- | ------------------ | ------------------- |
| value3      | 90_805_572         | 93_188_363          |
| perlin3     | 74_080_032         | 82_477_882          |
| valueCubic3 | 18_765_912         | 18_284_749          |

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
