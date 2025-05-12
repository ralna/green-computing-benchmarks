module blas_l1_benchmarks
    use benchmark_base, only: Benchmark
    use blas_interfaces, only: dasum
    use iso_fortran_env, only: int64, real64
    implicit none (external)
    private

    type, public, extends(Benchmark) :: DASUMBenchmark
        integer(int64) :: n

        double precision, dimension(:), allocatable :: x

        integer :: intx

        contains
            procedure :: run => run_dasum
            procedure :: call_benchmark => call_dasum
            procedure, nopass :: get_filename => dasum_filename
            procedure, nopass :: write_headers => dasum_headers

    end type DASUMBenchmark

contains
    character(len=64) function dasum_filename() result(filename)
        filename = "results_DASUM.csv"
        return
    end function dasum_filename

    subroutine run_dasum(self, blas_name, iunit)
        class(DASUMBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        integer, intent(in) :: iunit

        integer :: i
        real(real64) :: avg_gflops

        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = 1, 3
            self%n = 10**i

            allocate(self%x(self%n))
    
            call random_number(self%x)

            self%num_flops = self%n

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%x)
        end do

        write(iunit, '(A)') ''

    end subroutine run_dasum

    subroutine call_dasum(self)
        class(DASUMBenchmark), intent(inout) :: self
        call dasum(self%n, self%x, 1)
    end subroutine call_dasum

    subroutine dasum_headers(iunit)
        integer, intent(in) :: iunit
        integer :: i

        write(iunit, '(A)') 'Average performance of DASUM (GFLOPS/s)'
        write(iunit, '(A)') ',Vector size,'
        write(iunit, '(A)', advance='no') 'BLAS backend,'

        do i = 1, 3
            write(iunit, '(I6,A)', advance='no') 10**i, ','
        end do

        write(iunit, '(A)') ''
    end subroutine dasum_headers

end module blas_l1_benchmarks