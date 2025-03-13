module blas_l3_benchmarks
    use benchmark_base
    implicit none

    type, public, extends(Benchmark) :: DGEMMBenchmark
        integer :: m = 1000
        integer :: n = 1000
        integer :: k = 1000

        double precision :: alpha = 1.0
        double precision :: beta = 1.0

        double precision, dimension(:,:), allocatable :: A
        double precision, dimension(:,:), allocatable :: B
        double precision, dimension(:,:), allocatable :: C

        contains
            procedure :: setup => setup_dgemm
            procedure :: run => run_dgemm
        
    end type DGEMMBenchmark
    
contains
    subroutine setup_dgemm(self)
        class(DGEMMBenchmark), intent(inout) :: self
        
        allocate(self%A(self%m , self%n))
        allocate(self%B(self%n , self%k))
        allocate(self%C(self%k , self%n))
                
        call random_number(self%A)
        call random_number(self%B)
        call random_number(self%C)
    
        self%num_flops = 2 * self%m * self%n * self%k

        self%name = "DGEMM"
    
    end subroutine setup_dgemm
    
    subroutine run_dgemm(self)
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
            self%m&
        )
    end subroutine run_dgemm
end module blas_l3_benchmarks