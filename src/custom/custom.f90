module custom_benchmarks
    use benchmark_base, only: Benchmark
    use blas_interfaces, only: dgemv
    use iso_fortran_env, only: int64
    implicit none (external)
    private

    type, public, extends(Benchmark) :: NaiveMatmulBenchmark
        integer(int64) :: m = 1000
        integer(int64) :: n = 1000
        integer(int64) :: k = 1000

        double precision, dimension(:,:), allocatable :: A
        double precision, dimension(:,:), allocatable :: B
        double precision, dimension(:,:), allocatable :: C

        double precision :: alpha, beta

        contains
            procedure :: setup => setup_naive_matmul
            procedure :: run => run_naive_matmul

    end type NaiveMatmulBenchmark

contains
    subroutine setup_naive_matmul(self)
        class(NaiveMatmulBenchmark), intent(inout) :: self

        allocate(self%A(self%m, self%n))
        allocate(self%B(self%n, self%k))
        allocate(self%C(self%m, self%k))

        call random_number(self%A)
        call random_number(self%B)
        call random_number(self%C)

        self%alpha = 1.0
        self%beta = 0.0

        self%num_flops = 2 * self%m * self%n * self%k
        self%name = "Naive Matmul"

    end subroutine setup_naive_matmul

    subroutine run_naive_matmul(self)
        class(NaiveMatmulBenchmark), intent(inout) :: self
        call naive_matmul(&
            self%m,&
            self%n,&
            self%k,&
            self%alpha,&
            self%beta,&
            self%A,&
            self%B,&
            self%C&
        )

    end subroutine run_naive_matmul

    subroutine naive_matmul(m, n, k, alpha, beta, A, B, C)
        integer(int64), intent(in) :: m, n, k
        double precision, intent(in) :: alpha, beta
        double precision, dimension(:,:), intent(in) ::  A, B
        double precision, dimension(:,:), intent(inout) ::  C

        integer x, y, z

        do x = 1, m
            do y = 1, k
                do z = 1, n
                    C(x, y) = beta * C(x, y) + alpha * A(x, z) * B(z, y)
                end do
            end do
        end do
    
    end subroutine naive_matmul

end module custom_benchmarks