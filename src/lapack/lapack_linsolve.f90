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
            procedure, nopass :: get_filename => dgesv_filename
            procedure, nopass :: write_headers => dgesv_headers

    end type DGESVBenchmark

contains
    character(len=64) function dgesv_filename() result(filename)
        filename = "results_DGESV.csv"
        return
    end function dgesv_filename
    
    subroutine run_dgesv(self, blas_name, iunit)
        class(DGESVBenchmark), intent(inout) :: self

        character(len=16), intent(in) :: blas_name
        integer, intent(in) :: iunit
        
        integer :: i
        real(real64) :: avg_gflops

        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = 1, 3
            self%n = 10**i
            self%lda = 10**i
            self%ldb = 10**i

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

    subroutine dgesv_headers(iunit)
        integer, intent(in) :: iunit
        integer :: i

        write(iunit, '(A)') 'Average performance of DGESV (GFLOPS/s)'
        write(iunit, '(A)') ',Problem size,'
        write(iunit, '(A)', advance='no') 'BLAS backend,'

        do i = 1, 3
            write(iunit, '(I6,A)', advance='no') 10**i, ','
        end do

        write(iunit, '(A)') ''
    end subroutine dgesv_headers

end module lapack_linsolve_benchmarks