# CoSeC Green Computing Benchmarks

This repository contains a tool for benchmarking mathematical software libraries commonly used by the CCPs.

## Installation

The framework uses a [meson](https://mesonbuild.com/index.html) build system. It requires a Fortran compiler, as well as the [FlexiBLAS](https://www.mpi-magdeburg.mpg.de/projects/flexiblas) library.

To install the framework, run:

```
meson setup builddir --optimization=3
meson compile -C builddir
```

Optionally, GPU and MPI enabled benchmarks can be built by running
```
meson setup builddir --optimization=3 -Dgpu=true -Dmpi=true
meson compile -C builddir
```

The MPI benchmarks require SCALAPACK to be installed, and the GPU benchmarks require [SLATE](https://github.com/icl-utk-edu/slate/tree/master).

## Running

To run the benchmarks, run the `green_computing_benchmark` executable in `builddir`. By default this will write the results to `results.csv`. An alternative filename can be supplied as an additional command line argument.

 To change the BLAS backend benchmarks are run with, use:

```
FLEXIBLAS="YOUR_BLAS" ./green_computing_benchmark
```

Available BLAS backends are shown with `flexiblas list`

## Adding new benchmarks

Benchmarks are implemented as derived types which extend the `Benchmark` type. They must provide:

- A `num_flops` member, which is the total number of floating point operations performed during the benchmark,
- A `name` member,
- A `setup` subroutine. This is called once to intialise any data which is needed to run the benchmark,
- A `run` subroutine. This is the part of the benchmark which is actually timed, so should only include code which runs the calculation of interest.

Once the new type has been created, add the new benchmark to `benchmark_array` in `main.f90`.
