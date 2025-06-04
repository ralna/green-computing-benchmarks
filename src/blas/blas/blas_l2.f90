module blas_l2_benchmarks
    use benchmark_base, only: Benchmark
    use blas_interfaces, only: dgemv, sgemv
    use iso_fortran_env, only: int64, real64
    implicit none (external)
    private

    type, public, extends(Benchmark) :: DGEMVBenchmark
        integer(int64) :: m = 1000
        integer(int64) :: n = 1000

        double precision :: alpha = 1.0
        double precision :: beta = 1.0

        double precision, dimension(:,:), allocatable :: A
        double precision, dimension(:), allocatable :: x
        double precision, dimension(:), allocatable :: y

        contains
            procedure :: run => run_dgemv
            procedure :: call_benchmark => call_dgemv
    end type DGEMVBenchmark

    type, public, extends(Benchmark) :: SGEMVBenchmark
        integer(int64) :: m = 1000
        integer(int64) :: n = 1000

        real :: alpha = 1.0
        real :: beta = 1.0

        real, dimension(:,:), allocatable :: A
        real, dimension(:), allocatable :: x
        real, dimension(:), allocatable :: y

        contains
            procedure :: run => run_sgemv
            procedure :: call_benchmark => call_sgemv
    end type SGEMVBenchmark

contains
    subroutine run_dgemv(self, blas_name)
        class(DGEMVBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        integer :: i, iunit
        real(real64) :: avg_gflops

        self%name = "DGEMV"

        self%min_exp = 4
        self%max_exp = 15
        self%base = 2

        call self%open_results_file(iunit)
        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = self%min_exp, self%max_exp
            self%m = self%base**i
            self%n = self%base**i

            allocate(self%A(self%m , self%n))
            allocate(self%x(self%n))
            allocate(self%y(self%n))
    
            call random_number(self%A)
            call random_number(self%x)
            call random_number(self%y)
    
            self%num_flops = 2 * self%m * self%n
    
            avg_gflops = self%time_benchmark(100)
    
            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%A)
            deallocate(self%x)
            deallocate(self%y)
        end do

        write(iunit, '(A)') ''

        close(iunit)

    end subroutine run_dgemv

    subroutine call_dgemv(self)
        class(DGEMVBenchmark), intent(inout) :: self
        call dgemv(&
            "N",&
            self%m,&
            self%n,&
            self%alpha,&
            self%A,&
            self%m,&
            self%x,&
            1,&
            self%beta,&
            self%y,&
            1&
        )

    end subroutine call_dgemv

    subroutine run_sgemv(self, blas_name)
        class(SGEMVBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        integer :: i, iunit
        real(real64) :: avg_gflops

        self%name = "SGEMV"

        self%min_exp = 4
        self%max_exp = 15
        self%base = 2

        call self%open_results_file(iunit)
        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = self%min_exp, self%max_exp
            self%m = self%base**i
            self%n = self%base**i

            allocate(self%A(self%m , self%n))
            allocate(self%x(self%n))
            allocate(self%y(self%n))
    
            call random_number(self%A)
            call random_number(self%x)
            call random_number(self%y)
    
            self%num_flops = 2 * self%m * self%n
    
            avg_gflops = self%time_benchmark(100)
    
            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%A)
            deallocate(self%x)
            deallocate(self%y)
        end do

        write(iunit, '(A)') ''

        close(iunit)

    end subroutine run_sgemv

    subroutine call_sgemv(self)
        class(SGEMVBenchmark), intent(inout) :: self
        call sgemv(&
            "N",&
            self%m,&
            self%n,&
            self%alpha,&
            self%A,&
            self%m,&
            self%x,&
            1,&
            self%beta,&
            self%y,&
            1&
        )

    end subroutine call_sgemv
end module blas_l2_benchmarks