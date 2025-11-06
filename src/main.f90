program main
    use benchmark_types, only: BenchmarkContainer
    use benchmark_config, only: read_config
    use iso_fortran_env, only: real64, int64

    implicit none (external)

    integer :: i

    class(BenchmarkContainer), allocatable :: benchmark_array(:)

    character(len=64) :: filename
    character(len=16) :: blas_name
    integer :: iunit
    logical :: file_exists

    call read_config(benchmark_array)

    do i = 1, size(benchmark_array)
        call get_environment_variable("FLEXIBLAS", blas_name)

        call benchmark_array(i)%b%run(blas_name)
    end do

end program main
