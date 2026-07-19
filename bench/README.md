# `pure-noise` benchmarks

## Summary

These benchmarks measure single-threaded and parallel noise generation
performance for `pure-noise`, a pure Haskell noise library compiled with
the LLVM backend, against an optimized build of C++ FastNoiseLite (AVX2+FMA
w/ FP contraction).

At random access, `pure-noise` attains **roughly 70-100% of C++ throughput
depending on the algorithm. 2D cellular noise runs ~7% _faster_ than C++**,
largely a consequence of `pure-noise`'s implementation of cellular
configuration, which allows the configuration to optimize away in hot loops.

Grid-coherent access patterns measure lower for the simplex family.

Published numbers are collected with the `llvm-bench` cabal flag
(`--flags=+llvm-bench`).

If any passers-by notice errata in these documents or ways I could improve the
accuracy of the benchmarks, please don't hesitate to open an issue!

## Results (0.2.2.0, in-repo suite)

Collected 2026-07-19 on an i7-1370P (Raptor Cove P-cores) **pinned at 1.9 GHz**
(governor `performance`, turbo disabled) with GHC 9.12.2 / LLVM 19 / g++ 15.2.

The FNL comparison suite was pinned to one P-core for the comparison suite.

`% of FNL` ratios are intended to be clock-invariant; the absolute values/sec
figures scale with CPU clock, so expect proportionally higher throughput at
normal boost clocks and on more powerful hardware.

Full provenance in the `.meta` sidecars next to the tracked baselines.

### FastNoiseLite comparison (percent of FNL throughput; higher is better)

| algorithm        | grid | freq 0.01 | random |
| :--------------- | ---: | --------: | -----: |
| value 2D         |  77% |       77% |    77% |
| valueCubic 2D    |  81% |       81% |    81% |
| perlin 2D        |  88% |       88% |    88% |
| openSimplex2 2D  |  66% |       81% |    96% |
| superSimplex2 2D |  81% |       80% |    88% |
| cellular 2D      | 107% |      107% |   107% |
| value 3D         |  80% |       80% |    80% |
| valueCubic 3D    |  98% |       98% |    97% |
| perlin 3D        |  71% |       71% |    71% |
| openSimplex2 3D  |  86% |       77% |    78% |
| superSimplex2 3D |  95% |       86% |    68% |
| cellular 3D      |  90% |       90% |    90% |

Notes:

- `grid` = integer lattice
- `freq 0.01` - integer lattice scaled by frequency `0.01`, which is FNL's
- Branch-free algorithms (value, perlin, cellular) don't exhibit performance
variants against different input domain variances.
- Simplex spread appears to be branch predictability. C++ FNL gains up to 2x
on non-random inputs where pure-noise's more branchless lowering is stable even
against random samples.

### Single-thread values/sec (`pure-noise-bench`)

Cellular rows use `DistEuclidean`/`CellValue`.

#### 2D

| name          | Float (values/sec) | Double (values/sec) |
| :------------ | -----------------: | ------------------: |
| value2        |         64_407_126 |          68_260_653 |
| perlin2       |         61_301_663 |          65_143_707 |
| openSimplex2  |         25_982_291 |          27_045_472 |
| valueCubic2   |         22_743_642 |          23_403_842 |
| superSimplex2 |         17_167_069 |          17_762_669 |
| cellular2     |         16_025_950 |          16_007_044 |

#### 3D

| name          | Float (values/sec) | Double (values/sec) |
| :------------ | -----------------: | ------------------: |
| value3        |         34_673_623 |          35_929_146 |
| perlin3       |         29_325_590 |          30_432_482 |
| openSimplex3  |         10_975_857 |          10_922_644 |
| superSimplex3 |          9_232_128 |           9_166_843 |
| valueCubic3   |          7_453_365 |           7_278_612 |
| cellular3     |          5_238_497 |           5_061_911 |

### 2D parallel (`massiv`, 14 cores, pinned clocks)

Roughly **6-9x single-threaded throughput** on this machine; this is the
recommended path for bulk generation.

| name          | Float (values/sec) | Double (values/sec) |
| :------------ | -----------------: | ------------------: |
| value2        |        598_416_348 |         545_903_275 |
| perlin2       |        379_679_233 |         380_193_693 |
| openSimplex2  |        326_741_089 |         259_019_987 |
| valueCubic2   |        251_478_340 |         255_116_760 |
| superSimplex2 |        202_899_146 |         206_180_841 |
| cellular2     |        191_360_566 |         188_456_991 |

## Historical results (external NoiseBenchmarking tool, LLVM 15, i9-13900K)

The tables below are the original published comparison, collected before the
in-repo suite existed.

[FastNoiseLite (FNL)](https://github.com/Auburn/FastNoiseLite)
was benchmarked locally with Auburn's `NoiseBenchmarking` tool and lined up
against this library's results (compiled with the LLVM backend) by matching
coordinate methodology. They are retained until final re-collection.

**Direct comparison (integer-aligned grid, 512x512 for 2D, 64x64x64 for 3D):**

| Noise Type          | pure-noise (Float) | FastNoiseLite | % of FNL |
| :------------------ | :----------------- | :------------ | :------- |
| **Cellular 2D**     | 68_186_004         | 71_874_200    | 94.9%    |
| **SuperSimplex 2D** | 98_128_312         | 106_499_000   | 92.1%    |
| **Perlin 3D**       | 79_949_212         | 90_821_200    | 88.0%    |
| **OpenSimplex2 2D** | 116_801_665        | 133_444_000   | 87.5%    |
| **Value 3D**        | 96_972_157         | 111_514_000   | 87.0%    |
| **Perlin 2D**       | 158_245_846        | 182_558_000   | 86.7%    |
| **ValueCubic 2D**   | 62_931_346         | 73_415_900    | 85.7%    |
| **Value 2D**        | 178_258_969        | 211_971_000   | 84.1%    |
| **ValueCubic 3D**   | 19_674_751         | 23_361_200    | 84.2%    |

### Parallel Performance

The `massiv`-based parallel benchmarks demonstrate significantly higher throughput by leveraging multiple cores.
This is the intended path for high-performance, large-scale noise generation with this library.

For example, the parallel `massiv` 2D Perlin benchmark achieves **~1.73 billion values/sec**, over 9x the
single-threaded FNL result and 11x the single-threaded pure-noise result.

## Methodology & Reproducibility

### Measurement Approach

Benchmarks in the standard suite (`pure-noise-bench`) are run by mapping a noise
function over a 1 million element unboxed array of indices:

- This creates approximately 1-2ms of overhead
- Using index tuples increases the probability of hitting diverse code paths in
  the noise implementation. Some noise functions may skip certain computations
  when specific conditions are met relative to the input.
- Memory allocation is constant:
  - Float ~= 4.0MB (4 bytes x 1_000_000 elements)
  - Double ~= 8.0MB (8 bytes x 1_000_000 elements)

Benchmarks that use `massiv` demonstrate thread-level parallelism and are the intended path of use for
most purposes.

## Running the benchmarks

### Quick start

```sh
nix develop
# All of these emit CSV for ease-of-evaluation
./bench/results/collect-bench.sh
./bench/results/collect-bench.sh fnl # Shortcut for fnl comparisons
cabal bench pure-noise-fnl-bench --flags=+llvm-bench --benchmark-options='-p /perlin/ --stdev 2'
```

For serious comparisons, follow the brief guide in [FastNoiseLite Comparison]

#### Reading the output

Each `pure-noise` line ends with a ratio like `1.05x`, produced by
tasty-bench's `bcompare`: its mean time relative to the matching `fnl`
benchmark in the same group (lower is better; `0.95x` means pure-noise is 5%
_faster_ than FNL). "% of FNL throughput" is the reciprocal of the time
ratio. Ratios only print when the matching `fnl` leaf runs in the same
invocation, so filter by group (`-p /perlin/`), never by side. CSV output
(`--csv`) is plain tasty-bench data; `bench/results/vps.py` converts it to
values/second.

## FastNoiseLite comparison

### Comparison methodology

As of `0.2.2.0`, `pure-noise` uses a slightly different benchmarking methodology
for its comparisons. It creates an `unsafe` FFI binding to a C++ shim that
performs the noise-sum loop _in C++_ against a vendored `FastNoiseLite.h` (v1.1.1),
which should produce a much fairer result than either FFI for every point
(too noisy, even with `unsafe` calls) or the previous methodology (comparing
Google benchmark results with tasty/criterion manually).

`tasty-bench` is responsible for measuring the comparisons now, and most of the
custom infrastructure for results comparison is gone.

Cross-language comparison is built into the repo for reproducibility as of
`0.2.2.0`.

Published numbers are collected with the `llvm-bench` cabal flag on (LLVM
backend + `-optlc-fp-contract=fast` for the Haskell side); the collection
script passes it automatically.

#### C++ compiler flags

The shim builds with explicit flags rather than `-O3 -march=native`:

```
-O3 -std=c++14 -ffp-contract=fast -fstrict-overflow    (+ -march=x86-64-v3 on x86_64)
```

The goal is to measure FNL the way an ordinary Linux user's `-O3 -march=native`
build actually behaves, in a way that survives this repo's nix toolchain:

- nix's cc wrapper **strips `-march=native`** (`NIX_ENFORCE_NO_NATIVE=1`), which
  `-march=x86-64-v3` (AVX2+FMA, roughly what `native` selects on 2015+ x86)
  passes through Nix untouched.
- `-ffp-contract=fast` makes FP contraction explicit. It is the compiler
  default for gcc, clang, and MSVC in practice, and measured here it appears
  to be the _entirety_ of the speedup `-march=native` delivers on FMA-heavy
  kernels (perlin, cellular).
- nix hardening also injects `-fno-strict-overflow`. FNL's prime-multiplied
  hash coordinates overflow `int` by design. That is undefined behavior in
  C++ (g++ 15 warns at `FastNoiseLite.h:1658/1691/1724`), and gcc's
  UB-licensed loop optimizations are worth ~10-24% on cellular. Explicit
  `-fstrict-overflow` comes after the wrapper's injected flag and restores
  vanilla gcc behavior.

#### Manual matched-semantics comparison

These C++ flags grant the C++ library two licenses Haskell doesn't grant by
default: FP contraction and signed-overflow UB.

To compare pure codegen with those licenses revoked on the C++ side, temporarily
edit the `pure-noise-fnl-bench` `cxx-options` in `package.yaml` to end with
`-fwrapv -ffp-contract=off` and run `hpack` before rebuilding.

#### Measurement provenance

Every collection writes a `.meta` sidecar next to the CSV recording
toolchain versions, CPU model, governor, turbo state, AC power state, and
pinning. To promote a run to the tracked baseline:

```sh
cp bench/results/<stamp>-fnl.csv  bench/results/current-fnl.csv
cp bench/results/<stamp>-fnl.meta bench/results/current-fnl.meta
python3 bench/results/vps.py bench/results/current-fnl.csv
```

(and likewise without `-fnl` for the standard suite).

#### Backend note (NCG vs LLVM)

On the reference machine (i7-1370P, Raptor Cove P-core pinned at 1.9 GHz),
the native code generator measures ~2.85x slower than the LLVM backend on
`perlin2`, \~2.6-3.6x on `cellular2`, and \~1.4x on `openSimplex2`.

Ratios here and above are properties of this toolchain and microarchitecture,
with ~±1-2% run-to-run uncertainty under pinned clocks.

### System preparation

Make sure your system is quiet. Close all browsers an graphical applications,
confirm that the system is idling comfortably. If your DE/WM/etc. has a performance
mode, turn that on.

```bash
# 1. Set CPU governor to performance
echo performance | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor

# 2. Disable CPU frequency scaling (introduces unpredictable noise)
echo 0 | sudo tee /sys/devices/system/cpu/cpufreq/boost

# 3. Disable turbo boost (intel only)
echo 1 | sudo tee /sys/devices/system/cpu/intel_pstate/no_turbo
```

### Running the benchmark

You'll want to pin to a single core. Be sure this is a P-core, not an E-core.

```bash
cabal build bench:pure-noise-fnl-bench --flags=+llvm-bench
 # pinning to core 4, usually a P-core, unlikely to be a dumping ground for OS interrupts etc.
cabal exec -- taskset -c 4 "$(cabal list-bin pure-noise-fnl-bench --flags=+llvm-bench)"
```

### Restoring system performance settings

Don't forget to reset your system's perf settings (or just reboot, `/sys` changes
will reset):

```bash
# Restore power-saving CPU governor
echo powersave | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor

# Restore frequency scaling
echo 1 | sudo tee /sys/devices/system/cpu/cpufreq/boost

# Re-enable Turbo Boost (intel only)
echo 0 | sudo tee /sys/devices/system/cpu/intel_pstate/no_turbo
```

## NOTE: 0.2.x series benchmark accuracy

The 0.2.x series benchmarks had some issues that had been addressed. The most
severe correctness issue was unintentional integer-alignment in 3D benchmarks,
which didn't affect FNL comparisons.

The other significant issue was that my i9-13900K's ring was melting. I had to
RMA it later that year. So I'm a bit suspicious of the results - I just can't
really know for how long the ring was melting or how much it affected these
results, if at all.

Updated benchmarks on a new machine appear largely to confirm the original
findings. There are some performance result adjustments in both directions, which
I imagine is a result of both the ring melting and my recent attempts to further
increase benchmark precision.
