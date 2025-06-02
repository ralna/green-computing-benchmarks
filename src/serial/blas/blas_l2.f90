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
            procedure, nopass :: get_filename => dgemv_filename
            procedure, nopass :: write_headers => dgemv_headers

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
            procedure, nopass :: get_filename => sgemv_filename
            procedure, nopass :: write_headers => sgemv_headers

    end type SGEMVBenchmark

contains
    character(len=64) function dgemv_filename() result(filename)
        filename = "results_DGEMV.csv"
        return
    end function dgemv_filename

    subroutine run_dgemv(self, blas_name, iunit)
        class(DGEMVBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        integer, intent(in) :: iunit

        integer :: i
        real(real64) :: avg_gflops

        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = 1, 3
            self%m = 10**i
            self%n = 10**i

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

    subroutine dgemv_headers(iunit)
        integer, intent(in) :: iunit
        integer :: i

        write(iunit, '(3A)') 'Average performance of DGEMV (GFLOPS/s)'
        write(iunit, '(A)') ',Matrix size,'
        write(iunit, '(A)', advance='no') 'BLAS backend,'

        do i = 1, 3
            write(iunit, '(I6,A)', advance='no') 10**i, ','
        end do

        write(iunit, '(A)') ''
    end subroutine dgemv_headers

    character(len=64) function sgemv_filename() result(filename)
        filename = "results_SGEMV.csv"
        return
    end function sgemv_filename

    subroutine run_sgemv(self, blas_name, iunit)
        class(SGEMVBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        integer, intent(in) :: iunit

        integer :: i
        real(real64) :: avg_gflops

        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = 1, 3
            self%m = 10**i
            self%n = 10**i

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

    subroutine sgemv_headers(iunit)
        integer, intent(in) :: iunit
        integer :: i

        write(iunit, '(3A)') 'Average performance of SGEMV (GFLOPS/s)'
        write(iunit, '(A)') ',Matrix size,'
        write(iunit, '(A)', advance='no') 'BLAS backend,'

        do i = 1, 3
            write(iunit, '(I6,A)', advance='no') 10**i, ','
        end do

        write(iunit, '(A)') ''
    end subroutine sgemv_headers

end module blas_l2_benchmarks