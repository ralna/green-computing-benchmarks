project('green_computing_benchmark', 'fortran', 'c',
  version : '0.1.0',
  default_options : [
    'warning_level=3'
  ],
  meson_version: '>=1.3.2',
)
###################
# Setup           #
###################

blas_src = []
mpi_src = []
gpu_src = []
misc_src = []
deps = []

fc = meson.get_compiler('fortran')

###################
# Define sources  #
###################

subdir('src/blas')
subdir('src/mpi')
subdir('src/gpu')
subdir('src/misc')

###################
# Build library   #
###################

benchmark_base = library('benchmarkbase', 'src/benchmark_base.f90')

executable(
  'benchmark_misc', misc_src,
  dependencies: deps,
  link_language : 'fortran',
  install : true,
  link_with: benchmark_base
)

flexiblas_dep = dependency('flexiblas')
deps += flexiblas_dep

executable(
  'benchmark_blas', blas_src,
  dependencies: deps,
  link_language : 'fortran',
  install : true,
  link_with: benchmark_base
)

scalapack_dep = dependency('scalapack')
deps += scalapack_dep

mpi_dep = dependency('mpi')
deps += mpi_dep

executable(
  'benchmark_mpi', mpi_src,
  dependencies: deps,
  link_language : 'fortran',
  install : true,
  link_with: benchmark_base
)

slate_dep = dependency('slate')
deps += slate_dep

executable(
  'benchmark_gpu', gpu_src,
  dependencies: deps,
  link_language : 'fortran',
  install : true,
  link_with: benchmark_base
)
