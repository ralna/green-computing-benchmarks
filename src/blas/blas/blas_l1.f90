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
    end type DASUMBenchmark

    type, public, extends(Benchmark) :: SASUMBenchmark
        integer(int64) :: n

        real, dimension(:), allocatable :: x

        integer :: intx

        contains
            procedure :: run => run_sasum
            procedure :: call_benchmark => call_sasum
    end type SASUMBenchmark

    type, public, extends(Benchmark) :: DAXPYBenchmark
        integer(int64) :: n

        double precision, dimension(:), allocatable :: x
        double precision, dimension(:), allocatable :: y

        double precision :: alpha = 1.
        
        contains
            procedure :: run => run_daxpy
            procedure :: call_benchmark => call_daxpy
    end type DAXPYBenchmark

contains
    subroutine run_dasum(self, blas_name)
        class(DASUMBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name

        integer :: i, iunit
        real(real64) :: avg_gflops

        self%name = "DASUM"


        self%min_exp = 4
        self%max_exp = 18
        self%base = 2

        call self%open_results_file(iunit)
        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = self%min_exp, self%max_exp
            self%n = self%base**i

            allocate(self%x(self%n))
    
            call random_number(self%x)

            self%num_flops = self%n

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%x)
        end do

        write(iunit, '(A)') ''

    end subroutine run_dasum

    subroutine run_daxpy(self, blas_name)
        class(DAXPYBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name

        integer :: i, iunit
        real(real64) :: avg_gflops

        self%name = "DAXPY"

        self%min_exp = 4
        self%max_exp = 18
        self%base = 2

        call self%open_results_file(iunit)
        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = self%min_exp, self%max_exp
            self%n = self%base**i

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

        close(iunit)
    end subroutine run_daxpy

    subroutine run_sasum(self, blas_name)
        class(SASUMBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name

        integer :: i, iunit
        real(real64) :: avg_gflops

        self%name = "SASUM"

        self%min_exp = 4
        self%max_exp = 18
        self%base = 2

        call self%open_results_file(iunit)
        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = self%min_exp, self%max_exp
            self%n = self%base**i

            allocate(self%x(self%n))
    
            call random_number(self%x)

            self%num_flops = self%n

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%x)
        end do

        write(iunit, '(A)') ''

        close(iunit)
    end subroutine run_sasum
    
    subroutine call_dasum(self)
        class(DASUMBenchmark), intent(inout) :: self
        call dasum(self%n, self%x, 1)
    end subroutine call_dasum
    
    subroutine call_daxpy(self)
        class(DAXPYBenchmark), intent(inout) :: self
        call daxpy(self%n, self%alpha, self%x, 1, self%y, 1)
    end subroutine call_daxpy

    subroutine call_sasum(self)
        class(SASUMBenchmark), intent(inout) :: self
        call sasum(self%n, self%x, 1)
    end subroutine call_sasum

end module blas_l1_benchmarks