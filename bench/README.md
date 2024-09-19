# `pure-noise` benchmarks

Results suggest that the library is competitive with FastNoiseLite's C++ implementation when compiled with the LLVM backend.

Benchmarks were performed on an i9-13900K on a fedora 40 distrobox with LLVM 15 on the PATH.

## Notes

- Benchmarks are run by mapping a noise function over a 1 million element unboxed array of indices
  - This creates maybe ~1-2ms of overhead.
  - Using index tuples is intended to increase the probability that we hit as many possible code paths in the noise implementation. Some noise functions may skip certain computations when specific conditions are met relative to the input.
- Memory allocation should be constant.
  - Float ~= 4.0MB (4 bytes \* 1_000_000 elements)
  - Double ~= 8.0MB (8 bytes \* 1_000_000 elements)

## Reproducibility

There are two hacky scripts in this folder that can be used to bench with the exact arguments I've used.

- `results/collect-bench.sh` -- hacky script to document the parameters used to create benchmark results
- `results/vps.py` -- _very_ hacky script to calculate values/second numbers from `tasty-bench` csv output
