# CoSeC Green Computing Benchmarks

This repository contains a tool for benchmarking mathematical software libraries commonly used by the CCPs.

## Installation

The framework uses a [meson](https://mesonbuild.com/index.html) build system. It requires a Fortran compiler, as well as the [FlexiBLAS](https://www.mpi-magdeburg.mpg.de/projects/flexiblas) library.

To install the framework, run:

```
meson setup build --optimization=3
meson compile -C build
```

## Running

You can specify the benchmarks you wish to run using a [toml](https://toml.io/en/) configuration file. To add a function to benchmark, use the syntax:

```
[[benchmarks]]
name = "NAME"
m-sizes = [10, 20, 30]
n-sizes = [10, 50, 30]
k-sizes = [10, 50, 30]
```

Where "NAME" corresponds to the function you wish to benchmark. The list of available functions is below; unknown names will be ignored. `m-` `n-` and `k-sizes` correspond to the dimensions of the problems to benchmark on. 
Benchmarks will be run on the products of these arrays, for example the config above will run on 27 different sized problems.
Level 3 routines need m, n and k sizes to be specified, level 2 need m and n, and level 1 needs only m. Extra arrays in the config will be ignored.  

To run the benchmarks, run the `benchmark_blas` executable in `build`. A separate csv file will be created for each routine. Alternatively, you can run the `run_blas_benchmarks.sh` script, which will run your chosen benchmarks with all BLAS implementations available to FLEXIBLAS.

To manually change the BLAS backend benchmarks are run with, use:

```
FLEXIBLAS="YOUR_BLAS" ./build/benchmark_blas
```
Available BLAS backends are shown with `flexiblas list`.

## Available routines

### Level 1:
- DAXPY

### Level 2:
- DGEMV

### Level 3:
- DGEMM
- NAIVE
    - Naive non-BLAS matrix multiply
