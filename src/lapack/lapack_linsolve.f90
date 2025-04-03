module lapack_linsolve_benchmarks
    use benchmark_base, only: Benchmark
    use lapack_interfaces, only: dgesv
    use iso_fortran_env, only: int64
    implicit none (external)
    private

    type, public, extends(Benchmark) :: DGESVBenchmark
        integer(int64) :: n = 1000
        integer(int64) :: nrhs = 1
        integer(int64) :: lda = 1000
        integer(int64) :: ldb = 1000

        integer info

        double precision, dimension(:,:), allocatable :: a
        double precision, dimension(:,:), allocatable :: b
        double precision, dimension(:), allocatable :: ipiv

        contains
            procedure :: setup => setup_dgesv
            procedure :: run => run_dgesv

    end type DGESVBenchmark

contains
    subroutine setup_dgesv(self)
        class(DGESVBenchmark), intent(inout) :: self

        allocate(self%a(self%lda, self%n))
        allocate(self%b(self%ldb, self%nrhs))
        allocate(self%ipiv(self%n))

        call random_number(self%a)
        call random_number(self%b)
        
        ! From https://www.netlib.org/lapack/lug/node71.html#standardflopcount
        self%num_flops = 0.67 * self%n ** 3
        self%name = "DGESV"

    end subroutine setup_dgesv

    subroutine run_dgesv(self)
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

    end subroutine run_dgesv

end module lapack_linsolve_benchmarks