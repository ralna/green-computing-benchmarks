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
deps = []

fc = meson.get_compiler('fortran')

###################
# Define sources  #
###################

subdir('src')

###################
# Build library   #
###################


flexiblas_dep = dependency('flexiblas')

deps += flexiblas_dep

executable(
            'green_computing_benchmark', src,
            dependencies: deps,
            link_language : 'fortran',
            install : true,
            c_args: ['-O3']
        )
