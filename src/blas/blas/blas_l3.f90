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
            procedure, nopass :: get_filename => dgemm_filename
            procedure, nopass :: write_headers => dgemm_headers

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
            procedure, nopass :: get_filename => sgemm_filename
            procedure, nopass :: write_headers => sgemm_headers

    end type SGEMMBenchmark

contains
    character(len=64) function dgemm_filename() result(filename)
        filename = "results_DGEMM.csv"
        return
    end function dgemm_filename

    character(len=64) function sgemm_filename() result(filename)
        filename = "results_SGEMM.csv"
        return
    end function sgemm_filename

    subroutine run_dgemm(self, blas_name, iunit)
        class(DGEMMBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        integer, intent(in) :: iunit

        integer :: i
        real(real64) :: avg_gflops

        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = 4, 10
            self%m = 2**i
            self%n = 2**i
            self%k = 2**i

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
    end subroutine run_dgemm

    subroutine run_sgemm(self, blas_name, iunit)
        class(SGEMMBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        integer, intent(in) :: iunit

        integer :: i
        real(real64) :: avg_gflops

        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = 4, 10
            self%m = 2**i
            self%n = 2**i
            self%k = 2**i

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

    subroutine dgemm_headers(iunit)
        integer, intent(in) :: iunit
        integer :: i

        write(iunit, '(A)') 'Average performance of DGEMM (GFLOPS/s)'
        write(iunit, '(A)') ',Matrix size,'
        write(iunit, '(A)', advance='no') 'BLAS backend,'

        do i = 4, 10
            write(iunit, '(I6,A)', advance='no') 2**i, ','
        end do

        write(iunit, '(A)') ''
    end subroutine dgemm_headers

    subroutine sgemm_headers(iunit)
        integer, intent(in) :: iunit
        integer :: i

        write(iunit, '(A)') 'Average performance of SGEMM (GFLOPS/s)'
        write(iunit, '(A)') ',Matrix size,'
        write(iunit, '(A)', advance='no') 'BLAS backend,'

        do i = 4, 10
            write(iunit, '(I6,A)', advance='no') 2**i, ','
        end do

        write(iunit, '(A)') ''
    end subroutine sgemm_headers


end module blas_l3_benchmarks