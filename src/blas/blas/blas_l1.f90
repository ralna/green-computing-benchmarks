module blas_l1_benchmarks
    use benchmark_base, only: Benchmark
    use blas_interfaces, only: dasum, sasum, daxpy
    use iso_fortran_env, only: int64, real64
    implicit none (external)
    private

    type, public, extends(Benchmark) :: DASUMBenchmark
        integer(int64) :: n

        double precision, dimension(:), allocatable :: x

        contains
            procedure :: run => run_dasum
            procedure :: call_benchmark => call_dasum
            procedure, nopass :: get_filename => dasum_filename
            procedure, nopass :: write_headers => dasum_headers

    end type DASUMBenchmark

        type, public, extends(Benchmark) :: SASUMBenchmark
        integer(int64) :: n

        real, dimension(:), allocatable :: x

        integer :: intx

        contains
            procedure :: run => run_sasum
            procedure :: call_benchmark => call_sasum
            procedure, nopass :: get_filename => sasum_filename
            procedure, nopass :: write_headers => sasum_headers

    end type SASUMBenchmark

    type, public, extends(Benchmark) :: DAXPYBenchmark
        integer(int64) :: n

        double precision, dimension(:), allocatable :: x
        double precision, dimension(:), allocatable :: y

        double precision :: alpha = 1.0

        contains
            procedure :: run => run_daxpy
            procedure :: call_benchmark => call_daxpy
            procedure, nopass :: get_filename => daxpy_filename
            procedure, nopass :: write_headers => daxpy_headers

    end type DAXPYBenchmark

contains
    character(len=64) function dasum_filename() result(filename)
        filename = "results_DASUM.csv"
        return
    end function dasum_filename

    character(len=64) function daxpy_filename() result(filename)
        filename = "results_DAXPY.csv"
        return
    end function daxpy_filename

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

    subroutine run_daxpy(self, blas_name, iunit)
        class(DAXPYBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        integer, intent(in) :: iunit

        integer :: i
        real(real64) :: avg_gflops

        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = 3, 16
            self%n = 2**i

            allocate(self%x(self%n))
            allocate(self%y(self%n))
    
            call random_number(self%x)
            call random_number(self%y)

            self%num_flops = self%n

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%x)
            deallocate(self%y)
        end do

        write(iunit, '(A)') ''

    end subroutine run_daxpy

    subroutine call_dasum(self)
        class(DASUMBenchmark), intent(inout) :: self
        call dasum(self%n, self%x, 1)
    end subroutine call_dasum
    
    subroutine call_daxpy(self)
        class(DAXPYBenchmark), intent(inout) :: self
        call daxpy(self%n, self%alpha, self%x, 1, self%y, 1)
    end subroutine call_daxpy

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

    subroutine daxpy_headers(iunit)
        integer, intent(in) :: iunit
        integer :: i

        write(iunit, '(A)') 'Average performance of DAXPY (GFLOPS/s)'
        write(iunit, '(A)') ',Vector size,'
        write(iunit, '(A)', advance='no') 'BLAS backend,'

        do i = 3, 16
            write(iunit, '(I6,A)', advance='no') 2**i, ','
        end do

        write(iunit, '(A)') ''
    end subroutine daxpy_headers

    character(len=64) function sasum_filename() result(filename)
        filename = "results_SASUM.csv"
        return
    end function sasum_filename

    subroutine run_sasum(self, blas_name, iunit)
        class(SASUMBenchmark), intent(inout) :: self
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

    end subroutine run_sasum

    subroutine call_sasum(self)
        class(SASUMBenchmark), intent(inout) :: self
        call sasum(self%n, self%x, 1)
    end subroutine call_sasum

    subroutine sasum_headers(iunit)
        integer, intent(in) :: iunit
        integer :: i

        write(iunit, '(A)') 'Average performance of DASUM (GFLOPS/s)'
        write(iunit, '(A)') ',Vector size,'
        write(iunit, '(A)', advance='no') 'BLAS backend,'

        do i = 1, 3
            write(iunit, '(I6,A)', advance='no') 10**i, ','
        end do

        write(iunit, '(A)') ''
    end subroutine sasum_headers

end module blas_l1_benchmarks