module benchmark_base
    use iso_fortran_env, only: int64, real64
    implicit none (external)
    private

    type, public, abstract :: Benchmark
        integer(int64) :: num_flops
        character(len=32) :: name

        integer :: min_exp, max_exp, base

        contains
            procedure(run), deferred :: run
            procedure(call_benchmark), deferred :: call_benchmark
            procedure :: open_results_file
            procedure :: get_filename
            procedure :: time_benchmark
    end type Benchmark

    type, public :: BenchmarkContainer
        class(Benchmark), allocatable :: b
    end type BenchmarkContainer

    abstract interface
        subroutine run(self, blas_name)
            import
            implicit none (external)
            class(Benchmark), intent(inout) :: self
            character(len=16), intent(in) :: blas_name
        end subroutine run

        subroutine call_benchmark(self)
            import
            implicit none (external)
            class(Benchmark), intent(inout) :: self
        end subroutine call_benchmark
    end interface

    contains
        character(len=64) function get_filename(self) result(filename)
            class(Benchmark), intent(inout) :: self

            filename = "results_"//trim(self%name)//".csv"
            return
        end function get_filename

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

        subroutine open_results_file(self, iunit)
            class(Benchmark), intent(inout) :: self
            integer, intent(out) :: iunit

            integer :: i
            character(len = 64) :: filename
            logical :: file_exists

            filename = self%get_filename()

            inquire(file=filename, exist=file_exists)
            open(newunit=iunit, file=filename, position="append")
    
            if (.not. file_exists) then
                write(iunit, '(3A)') 'Average performance of', self%name, '(GFLOPS/s)'
                write(iunit, '(A)') ',Problem size,'
                write(iunit, '(A)', advance='no') 'BLAS backend,'
    
                do i = self%min_exp, self%max_exp
                    write(iunit, '(I6,A)', advance='no') self%base**i, ','
                end do
    
                write(iunit, '(A)') ''
                end if
        end subroutine open_results_file

end module benchmark_base
