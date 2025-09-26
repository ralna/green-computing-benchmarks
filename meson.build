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

build_gpu = get_option('gpu')
build_mpi = get_option('mpi')

###################
# Define sources  #
###################

subdir('src/blas')
subdir('src/mpi')
subdir('src/gpu')
subdir('src/misc')

tomlf_dep = dependency(
  'toml-f',
  version: '>=0.2.0',
  fallback: ['toml-f', 'tomlf_dep'],
  default_options: ['default_library=static'],
)

deps += tomlf_dep

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

if build_mpi
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
endif

if build_gpu
  slate_dep = dependency('slate')
  deps += slate_dep
  
  executable(
    'benchmark_gpu', gpu_src,
    dependencies: deps,
    link_language : 'fortran',
    install : true,
    link_with: benchmark_base
  )
endif

