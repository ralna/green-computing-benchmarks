program main
    use benchmark_base
    use blas_l2_benchmarks
    use blas_l3_benchmarks
    implicit none
    
    external ddgemm

    real :: sum_gflops = 0
    real gflops

    integer :: n_iters = 100
    integer :: i, j

    class(Benchmark), allocatable :: b

    real :: start_time, end_time

    class(BenchmarkContainer), allocatable :: benchmark_array(:)

    allocate(benchmark_array(2))
    allocate(DGEMMBenchmark::benchmark_array(1)%b)
    allocate(DGEMVBenchmark::benchmark_array(2)%b)

    do i = 1, size(benchmark_array)
        b = benchmark_array(i)%b
        call b%setup()

        do j = 1, n_iters
            call cpu_time(start_time)
            call b%run()
            call cpu_time(end_time)

            gflops = b%num_flops / (1000**3 * (end_time - start_time))

            sum_gflops = sum_gflops + gflops
        end do

    print *, "Average performace of ", b%name, ":", sum_gflops / n_iters, "GFLOPS/s"

    end do


end program main
