project('green_computing_benchmark', 'fortran',
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
headers = []
include_dirs = []
deps = []

fs = import('fs')

fc = meson.get_compiler('fortran')

###################
# Define sources  #
###################

subdir('src')

###################
# Build library   #
###################

blas_name = get_option('blas')
lapack_name = get_option('lapack')

blas_dep = dependency(blas_name)
lapack_dep = dependency(lapack_name)

deps += blas_dep
deps += lapack_dep

executable(
            'green_computing_benchmark', src,
            dependencies: deps,
            link_language : 'fortran',
            install : true,
        )