program main
    use benchmark_base, only: Benchmark, BenchmarkContainer
    use slate_benchmarks, only: SLATE_MULT_DBenchmark, SLATE_MULT_SBenchmark
    use iso_fortran_env, only: real64, int64
    use iso_c_binding, only: c_int
    use mpi

    implicit none (external)

    integer :: i

    class(Benchmark), allocatable :: b

    class(BenchmarkContainer), allocatable :: benchmark_array(:)

    character(len=64) :: filename
    character(len=16) :: blas_name
    integer :: iunit
    logical :: file_exists

    integer(kind=c_int) :: provided, ierr

    allocate(benchmark_array(2))
    allocate(SLATE_MULT_DBenchmark::benchmark_array(1)%b)
    allocate(SLATE_MULT_SBenchmark::benchmark_array(2)%b)

    call MPI_Init_thread( MPI_THREAD_MULTIPLE, provided, ierr )

    do i = 1, size(benchmark_array)
        b = benchmark_array(i)%b

        call get_environment_variable("FLEXIBLAS", blas_name)

        call b%run(blas_name)
    end do

end program main
