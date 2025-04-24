module custom_benchmarks
    use benchmark_base, only: Benchmark
    use blas_interfaces, only: dgemv
    use iso_fortran_env, only: int64, real64
    implicit none (external)
    private

    type, public, extends(Benchmark) :: NaiveMatmulBenchmark
        integer(int64) :: m
        integer(int64) :: n
        integer(int64) :: k

        double precision, dimension(:,:), allocatable :: A
        double precision, dimension(:,:), allocatable :: B
        double precision, dimension(:,:), allocatable :: C

        double precision :: alpha, beta

        contains
            procedure :: run => run_naive_matmul
            procedure :: call_benchmark => call_naive_matmul
            procedure, nopass :: get_filename => naive_matmul_filename
            procedure, nopass :: write_headers => naive_matmul_headers

    end type NaiveMatmulBenchmark

contains
    character(len=64) function naive_matmul_filename() result(filename)
        filename = "results_NAIVE_MATMUL.csv"
        return
    end function naive_matmul_filename

    subroutine run_naive_matmul(self, blas_name, iunit)
        class(NaiveMatmulBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        integer, intent(in) :: iunit

        integer :: i
        real(real64) :: avg_gflops

        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = 1, 3
            self%m = 10**i
            self%n = 10**i
            self%k = 10**i

            allocate(self%A(self%m, self%n))
            allocate(self%B(self%n, self%k))
            allocate(self%C(self%m, self%k))
    
            call random_number(self%A)
            call random_number(self%B)
            call random_number(self%C)
    
            self%alpha = 1.0
            self%beta = 0.0
    
            self%num_flops = 2 * self%m * self%n * self%k

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%A)
            deallocate(self%B)
            deallocate(self%C)
        end do

        write(iunit, '(A)') ''
    end subroutine run_naive_matmul

    subroutine call_naive_matmul(self)
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
    end subroutine call_naive_matmul

    subroutine naive_matmul(m, n, k, alpha, beta, A, B, C)
        integer(int64), intent(in) :: m, n, k
        double precision, intent(in) :: alpha, beta
        double precision, dimension(:,:), intent(in) ::  A, B
        double precision, dimension(:,:), intent(inout) ::  C

        integer(int64) x, y, z

        do x = 1, m
            do y = 1, k
                do z = 1, n
                    C(x, y) = beta * C(x, y) + alpha * A(x, z) * B(z, y)
                end do
            end do
        end do
    
    end subroutine naive_matmul

    subroutine naive_matmul_headers(iunit)
        integer, intent(in) :: iunit
        integer :: i

        write(iunit, '(A)') 'Average performance of Naive MATMUL (GFLOPS/s)'
        write(iunit, '(A)') ',Matrix size,'
        write(iunit, '(A)', advance='no') 'BLAS backend,'

        do i = 1, 3
            write(iunit, '(I6,A)', advance='no') 10**i, ','
        end do

        write(iunit, '(A)') ''
    end subroutine naive_matmul_headers



end module custom_benchmarks