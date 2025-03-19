# CoSeC Green Computing Benchmarks

This repository contains a tool for benchmarking mathematical software libraries commonly used by the CCPs.

## Installation

The framework uses a [meson](https://mesonbuild.com/index.html) build system. It requires a Fortran compiler, as well as the BLAS and LAPACK libraries. OpenBLAS is used by default.

To install the framework, run:

```
meson setup builddir
```

At this stage you can specify your BLAS and LAPACK implentations by providing the arguments `Dblas=blas_lib` and `Dlapack=lapack_lib`. `blas_lib` and `lapack_lib` will be found by meson using `pkg-config`.

Then compile with: 

```
meson compile -C builddir
```

## Running

To run the benchmarks, run the `green_computing_benchmark` executable in `builddir`. This will write the results to `results.csv`.

## Adding new benchmarks

Benchmarks are implemented as derived types which extend the `Benchmark` type. They must provide:

- A `num_flops` member, which is the total number of floating point operations performed during the benchmark,
- A `name` member,
- A `setup` subroutine. This is called once to intialise any data which is needed to run the benchmark,
- A `run` subroutine. This is the part of the benchmark which is actually timed, so should only include code which runs the calculation of interest.

Once the new type has been created, add the new benchmark to `benchmark_array` in `main.f90`.
