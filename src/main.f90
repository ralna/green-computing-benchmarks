program main
    use benchmark_base, only: Benchmark, BenchmarkContainer
    ! use blas_l1_benchmarks, only: DASUMBenchmark
    ! use blas_l2_benchmarks, only: DGEMVBenchmark
    use blas_l3_benchmarks, only: DGEMMBenchmark
    ! use custom_benchmarks, only: NaiveMatmulBenchmark
    ! use lapack_linsolve_benchmarks, only: DGESVBenchmark
    use iso_fortran_env, only: real64, int64

    implicit none (external)

    real(real64) :: sum_flops, flops, avg_gflops

    integer :: n_iters = 100
    integer :: i, j

    class(Benchmark), allocatable :: b

    integer(int64) :: start_count, end_count
    integer(int64) :: count_rate, count_max

    real(real64) :: elapsed_time

    class(BenchmarkContainer), allocatable :: benchmark_array(:)

    character(len=64) :: filename
    character(len=16) :: blas_name
    integer blas_name_status
    integer :: iunit

    allocate(benchmark_array(1))
    allocate(DGEMMBenchmark::benchmark_array(1)%b)
    ! allocate(DGEMVBenchmark::benchmark_array(2)%b)
    ! allocate(DASUMBenchmark::benchmark_array(3)%b)
    ! allocate(DGESVBenchmark::benchmark_array(4)%b)
    ! allocate(NaiveMatmulBenchmark::benchmark_array(5)%b)

    call get_command_argument(1, blas_name, status=blas_name_status)

    if (blas_name_status > 0) then
        blas_name = "Unknown BLAS"
    end if

    iunit = 1

    do i = 1, size(benchmark_array)
        b = benchmark_array(i)%b
        filename = b%get_filename()
        open(iunit, file=filename)
        call b%run(blas_name)
        close(iunit)

    end do

    close(iunit)

end program main
