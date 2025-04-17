program main
    use benchmark_base, only: Benchmark, BenchmarkContainer
    use blas_l1_benchmarks, only: DASUMBenchmark
    use blas_l2_benchmarks, only: DGEMVBenchmark
    use blas_l3_benchmarks, only: DGEMMBenchmark
    use custom_benchmarks, only: NaiveMatmulBenchmark
    use lapack_linsolve_benchmarks, only: DGESVBenchmark
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
    integer filename_status
    integer :: iunit

    allocate(benchmark_array(5))
    allocate(DGEMMBenchmark::benchmark_array(1)%b)
    allocate(DGEMVBenchmark::benchmark_array(2)%b)
    allocate(DASUMBenchmark::benchmark_array(3)%b)
    allocate(DGESVBenchmark::benchmark_array(4)%b)
    allocate(NaiveMatmulBenchmark::benchmark_array(5)%b)

    call get_command_argument(1, filename, status=filename_status)

    if (filename_status > 0) then
        filename = "results.csv"
    end if

    iunit = 1
    open(iunit, file=filename, status='replace', action='write')

    write(iunit, '(A)') 'Benchmark Name,Average performance (GFLOPS/s)'

    do i = 1, size(benchmark_array)
        b = benchmark_array(i)%b
        call b%setup()

        sum_flops = 0

        do j = 1, n_iters
            call system_clock(start_count, count_rate, count_max)
            call b%run()
            call system_clock(end_count, count_rate, count_max)
            
            !Time (s)
            elapsed_time = real(end_count - start_count) / real(count_rate)

            !FLOPS/s
            flops = real(b%num_flops) / (elapsed_time)

            sum_flops = sum_flops + flops

        end do
        
        avg_gflops = sum_flops / (1000.0**3 * n_iters)

        write(iunit, '(A, A, F11.7)') b%name, ',', avg_gflops

    end do

    close(iunit)

end program main
