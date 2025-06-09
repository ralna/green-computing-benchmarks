module lapack_linsolve_benchmarks
    use benchmark_base, only: Benchmark
    use lapack_interfaces, only: dgesv
    use iso_fortran_env, only: int64, real64
    implicit none (external)
    private

    type, public, extends(Benchmark) :: DGESVBenchmark
        integer(int64) :: n
        integer(int64) :: nrhs = 1
        integer(int64) :: lda
        integer(int64) :: ldb

        integer info

        double precision, dimension(:,:), allocatable :: a
        double precision, dimension(:,:), allocatable :: b
        double precision, dimension(:), allocatable :: ipiv
        
        contains
            procedure :: run => run_dgesv
            procedure :: call_benchmark => call_dgesv
    end type DGESVBenchmark

contains
    subroutine run_dgesv(self, blas_name)
        class(DGESVBenchmark), intent(inout) :: self

        character(len=16), intent(in) :: blas_name
        
        integer :: i, iunit
        real(real64) :: avg_gflops

        self%name = "DGESV"

        self%min_exp = 4
        self%max_exp = 16
        self%base = 2

        call self%open_results_file(iunit)
        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = self%min_exp, self%max_exp
            self%n = self%base**i
            self%lda = self%base**i
            self%ldb = self%base**i

            allocate(self%a(self%lda, self%n))
            allocate(self%b(self%ldb, self%nrhs))
            allocate(self%ipiv(self%n))
    
            call random_number(self%a)
            call random_number(self%b)
            
            ! From https://www.netlib.org/lapack/lug/node71.html#standardflopcount
            self%num_flops = 0.67 * self%n ** 3

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%a)
            deallocate(self%b)
            deallocate(self%ipiv)

        end do

        close(iunit)
    end subroutine run_dgesv

    subroutine call_dgesv(self)
        class(DGESVBenchmark), intent(inout) :: self
        call dgesv(&
            self%n,&
            self%nrhs,&
            self%a,&
            self%lda,&
            self%ipiv,&
            self%b,&
            self%ldb,&
            self%info&
        )

    end subroutine call_dgesv
end module lapack_linsolve_benchmarks