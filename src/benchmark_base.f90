module benchmark_base
    use iso_fortran_env, only: int64, real64
    implicit none (external)
    private

    type, public, abstract :: Benchmark
        integer(int64) :: num_flops
        contains
            procedure(run), deferred :: run
            procedure(call_benchmark), deferred :: call_benchmark
            procedure(get_filename), nopass, deferred :: get_filename
            procedure(write_headers), nopass, deferred :: write_headers
            procedure :: time_benchmark
    end type Benchmark

    type, public :: BenchmarkContainer
        class(Benchmark), allocatable :: b
    end type BenchmarkContainer

    abstract interface
        subroutine run(self, blas_name, iunit)
            import
            implicit none (external)
            class(Benchmark), intent(inout) :: self
            character(len=16), intent(in) :: blas_name
            integer, intent(in) :: iunit
        end subroutine run

        subroutine call_benchmark(self)
            import
            implicit none (external)
            class(Benchmark), intent(inout) :: self
        end subroutine call_benchmark

        character(len=64) function get_filename() result(filename)
            import
            implicit none (external)
        end function get_filename

        subroutine write_headers(iunit)
            import
            implicit none (external)
            integer, intent(in) :: iunit
        end subroutine write_headers
    end interface

    contains
        function time_benchmark(self, n_iters) result(avg_gflops)
            class(Benchmark), intent(inout) :: self
            integer, intent(in) :: n_iters

            integer(int64) :: start_count, end_count
            integer(int64) :: count_rate, count_max

            real(real64) :: elapsed_time

            real(real64) :: sum_flops, flops, avg_gflops

            integer :: i

            sum_flops = 0

            do i = 1, n_iters
                call system_clock(start_count, count_rate, count_max)
                call self%call_benchmark()
                call system_clock(end_count, count_rate, count_max)

                !Time (s)
                elapsed_time = real(end_count - start_count) / real(count_rate)

                !FLOPS/s
                flops = real(self%num_flops) / (elapsed_time)

                sum_flops = sum_flops + flops

            end do

            avg_gflops = sum_flops / (1000.0**3 * n_iters)
            return
        end function time_benchmark

end module benchmark_base
