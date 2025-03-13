program main
    use benchmark_base
    use blas_l2_benchmarks
    use blas_l3_benchmarks
    use iso_fortran_env, only: int64

    implicit none
    
    external ddgemm

    real :: sum_gflops = 0
    real gflops

    integer :: n_iters = 100
    integer :: i, j

    class(Benchmark), allocatable :: b

    integer(int64) start_count, end_count
    integer(int64) count_rate, count_max

    real elapsed_time

    class(BenchmarkContainer), allocatable :: benchmark_array(:)

    allocate(benchmark_array(2))
    allocate(DGEMMBenchmark::benchmark_array(1)%b)
    allocate(DGEMVBenchmark::benchmark_array(2)%b)

    do i = 1, size(benchmark_array)
        b = benchmark_array(i)%b
        call b%setup()

        do j = 1, n_iters
            call system_clock(start_count, count_rate, count_max)
            call b%run()
            call system_clock(end_count, count_rate, count_max)

            elapsed_time = real(end_count - start_count) / real(count_rate)

            gflops = b%num_flops / (1000**3 * elapsed_time)

            sum_gflops = sum_gflops + gflops
        end do

    print *, "Average performace of ", b%name, ":", sum_gflops / n_iters, "GFLOPS/s"

    end do


end program main
