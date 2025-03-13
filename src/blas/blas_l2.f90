module blas_l2_benchmarks
    use benchmark_base
    implicit none

    type, public, extends(Benchmark) :: DGEMVBenchmark
        integer :: m = 1000
        integer :: n = 1000

        double precision :: alpha = 1.0
        double precision :: beta = 1.0

        double precision, dimension(:,:), allocatable :: A
        double precision, dimension(:), allocatable :: x
        double precision, dimension(:), allocatable :: y

        contains
            procedure :: setup => setup_dgemv
            procedure :: run => run_dgemv
        
    end type DGEMVBenchmark
    
contains
    subroutine setup_dgemv(self)
        class(DGEMVBenchmark), intent(inout) :: self
        
        allocate(self%A(self%m , self%n))
        allocate(self%x(self%n))
        allocate(self%y(self%m))
                
        call random_number(self%A)
        call random_number(self%x)
        call random_number(self%y)
    
        self%num_flops = 2 * self%m * self%n
        self%name = "DGEMV"
    
    end subroutine setup_dgemv
    
    subroutine run_dgemv(self)
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

    end subroutine run_dgemv
    
end module blas_l2_benchmarks