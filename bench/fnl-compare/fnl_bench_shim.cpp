// Bulk-loop shim over the vendored FastNoiseLite.h (./cbits/FastNoiseLite.h)
// to provide a fair comparison to FNL.
//
// Sticking the loop in C++ should prevent FFI overhead from skewing results. 
// Unsafe calls should have double-digit nanosecond overhead, so the signal from 
// the bench itself should dominate.
//
// tasty-bench handles timing and statistics; simpler and less numerically fraught 
// than trying to cross-compare Google benchmark to tasty/criterion.
//
// This should be compiled the way a user would build FNL: -O3 -march=native. We
// want to give C++ every edge it can have for a meaningful comparison.
//
// NOTE: noiseType convention is:
//   0=OpenSimplex2 
//   1=OpenSimplex2S 
//   2=Cellular 
//   3=Perlin 
//   4=ValueCubic 
//   5=Value
#include "FastNoiseLite.h"

static FastNoiseLite mk(int noiseType, int seed, float freq) {
    FastNoiseLite fnl(seed);
    fnl.SetNoiseType(static_cast<FastNoiseLite::NoiseType>(noiseType));
    // pure-noise's loop multiplies every coordinate by a runtime k; FNL pays
    // the same multiply here via mFrequency.
    fnl.SetFrequency(freq);
    if (noiseType == 2) { // match audit-bench: Euclidean distance, Distance result
        fnl.SetCellularDistanceFunction(FastNoiseLite::CellularDistanceFunction_Euclidean);
        fnl.SetCellularReturnType(FastNoiseLite::CellularReturnType_Distance);
    }
    return fnl;
}

extern "C" {

float fnl_bench_sum2(int noiseType, int seed, float freq, int n,
                     const float* __restrict xs, const float* __restrict ys) {
    FastNoiseLite fnl = mk(noiseType, seed, freq);
    float acc = 0.0f;
    for (int i = 0; i < n; i++) acc += fnl.GetNoise(xs[i], ys[i]);
    return acc;
}

float fnl_bench_sum3(int noiseType, int seed, float freq, int n,
                     const float* __restrict xs, const float* __restrict ys, const float* __restrict zs) {
    FastNoiseLite fnl = mk(noiseType, seed, freq);
    float acc = 0.0f;
    for (int i = 0; i < n; i++) acc += fnl.GetNoise(xs[i], ys[i], zs[i]);
    return acc;
}

} // extern "C"
