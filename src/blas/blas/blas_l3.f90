module blas_l3_benchmarks
    use benchmark_base, only: Benchmark
    use blas_interfaces, only: dgemm, sgemm
    use iso_fortran_env, only: int64, real64
    implicit none (external)
    private

    type, public, extends(Benchmark) :: DGEMMBenchmark
        integer(int64) :: m
        integer(int64) :: n
        integer(int64) :: k

        double precision :: alpha = 1.0
        double precision :: beta = 1.0

        double precision, dimension(:,:), allocatable :: A
        double precision, dimension(:,:), allocatable :: B
        double precision, dimension(:,:), allocatable :: C

        contains
            procedure :: run => run_dgemm
            procedure :: call_benchmark => call_dgemm
    end type DGEMMBenchmark

    type, public, extends(Benchmark) :: SGEMMBenchmark
        integer(int64) :: m
        integer(int64) :: n
        integer(int64) :: k

        real :: alpha = 1.0
        real :: beta = 1.0

        real, dimension(:,:), allocatable :: A
        real, dimension(:,:), allocatable :: B
        real, dimension(:,:), allocatable :: C

        contains
            procedure :: run => run_sgemm
            procedure :: call_benchmark => call_sgemm
    end type SGEMMBenchmark

contains
    subroutine run_dgemm(self, blas_name)
        class(DGEMMBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name

        integer :: i, iunit
        real(real64) :: avg_gflops

        self%name = "DGEMM"

        call self%open_results_file(iunit)

        write(iunit, '(2A)', advance='no') blas_name, ','

        self%min_exp = 4
        self%max_exp = 10
        self%base = 2

        do i = self%min_exp, self%max_exp
            self%m = self%base**i
            self%n = self%base**i
            self%k = self%base**i

            allocate(self%A(self%m , self%n))
            allocate(self%B(self%n , self%k))
            allocate(self%C(self%k , self%n))

            call random_number(self%A)
            call random_number(self%B)
            call random_number(self%C)

            self%num_flops = 2 * self%m * self%n * self%k

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%A)
            deallocate(self%B)
            deallocate(self%C)
        end do

        write(iunit, '(A)') ''

        close(iunit)

    end subroutine run_dgemm

    subroutine run_sgemm(self, blas_name)
        class(SGEMMBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        
        integer :: i, iunit
        real(real64) :: avg_gflops

        self%name = "SGEMM"

        call self%open_results_file(iunit)

        write(iunit, '(2A)', advance='no') blas_name, ','

        self%min_exp = 4
        self%max_exp = 10
        self%base = 2

        do i = self%min_exp, self%max_exp
            self%m = self%base**i
            self%n = self%base**i
            self%k = self%base**i

            allocate(self%A(self%m , self%n))
            allocate(self%B(self%n , self%k))
            allocate(self%C(self%k , self%n))

            call random_number(self%A)
            call random_number(self%B)
            call random_number(self%C)

            self%num_flops = 2 * self%m * self%n * self%k

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%A)
            deallocate(self%B)
            deallocate(self%C)
        end do

        write(iunit, '(A)') ''
        
        close(iunit)

    end subroutine run_sgemm

    subroutine call_dgemm(self)
        class(DGEMMBenchmark), intent(inout) :: self
        call dgemm(&
            "N",&
            "N",&
            self%m,&
            self%n,&
            self%k,&
            self%alpha,&
            self%A,&
            self%m,&
            self%B,&
            self%k,&
            self%beta,&
            self%C,&
            self%n&
        )
    end subroutine call_dgemm

    subroutine call_sgemm(self)
        class(SGEMMBenchmark), intent(inout) :: self
        call sgemm(&
            "N",&
            "N",&
            self%m,&
            self%n,&
            self%k,&
            self%alpha,&
            self%A,&
            self%m,&
            self%B,&
            self%k,&
            self%beta,&
            self%C,&
            self%n&
        )
    end subroutine call_sgemm
end module blas_l3_benchmarks