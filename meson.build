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

src = []
deps = []

fc = meson.get_compiler('fortran')

###################
# Define sources  #
###################

subdir('src')

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

flexiblas_dep = dependency('flexiblas')
deps += flexiblas_dep

executable(
  'benchmark_blas', src,
  dependencies: deps,
  link_language : 'fortran',
  install : true,
)
