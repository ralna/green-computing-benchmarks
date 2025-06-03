program main
    use benchmark_base, only: Benchmark, BenchmarkContainer
    use blas_l1_benchmarks, only: DASUMBenchmark, SASUMBenchmark, DAXPYBenchmark
    use blas_l2_benchmarks, only: DGEMVBenchmark, SGEMVBenchmark
    use blas_l3_benchmarks, only: DGEMMBenchmark, SGEMMBenchmark
    use lapack_linsolve_benchmarks, only: DGESVBenchmark
    use iso_fortran_env, only: real64, int64

    implicit none (external)

    integer :: i

    class(Benchmark), allocatable :: b

    class(BenchmarkContainer), allocatable :: benchmark_array(:)

    character(len=64) :: filename
    character(len=16) :: blas_name
    integer :: iunit
    logical :: file_exists

    allocate(benchmark_array(7))
    allocate(DGEMMBenchmark::benchmark_array(1)%b)
    allocate(DGEMVBenchmark::benchmark_array(2)%b)
    allocate(SGEMVBenchmark::benchmark_array(3)%b)
    allocate(DASUMBenchmark::benchmark_array(4)%b)
    allocate(SASUMBenchmark::benchmark_array(5)%b)
    allocate(DGESVBenchmark::benchmark_array(6)%b)
    allocate(DAXPYBenchmark::benchmark_array(7)%b)

    do i = 1, size(benchmark_array)
        b = benchmark_array(i)%b

        call get_environment_variable("FLEXIBLAS", blas_name)

        call b%run(blas_name)
    end do

end program main
