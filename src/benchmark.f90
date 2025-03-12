module benchmarks
    implicit none

    type, abstract :: Benchmark
        integer :: num_flops
        character(len=16) :: name
    contains
        procedure(setup_interface), deferred :: setup
        procedure(run_interface), deferred :: run
    end type Benchmark

    type, public :: BenchmarkContainer
        class(Benchmark), allocatable :: b
    end type BenchmarkContainer

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
    
    abstract interface 
        subroutine setup_interface(self)
            import
            class(Benchmark), intent(inout) :: self
        end subroutine setup_interface

        subroutine run_interface(self)
            import
            class(Benchmark), intent(inout) :: self
        end subroutine run_interface
    end interface
    
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
    
end module benchmarks

program main
    use benchmarks
    implicit none
    
    external ddgemm

    real :: sum_gflops = 0
    real gflops

    integer :: n_iters = 100
    integer :: i, j

    class(Benchmark), allocatable :: b

    real :: start_time, end_time

    class(BenchmarkContainer), allocatable :: benchmark_array(:)

    allocate(benchmark_array(2))
    allocate(DGEMMBenchmark::benchmark_array(1)%b)
    allocate(DGEMVBenchmark::benchmark_array(2)%b)

    do i = 1, size(benchmark_array)
        b = benchmark_array(i)%b
        call b%setup()
        do j = 1, n_iters
            call cpu_time(start_time)
            call b%run()
            call cpu_time(end_time)
            gflops = b%num_flops / (1000**3 * (end_time - start_time))
    
            sum_gflops = sum_gflops + gflops
        end do
    print *, "Average performace of ", b%name, ":", sum_gflops / n_iters, "GFLOPS/s"

    end do


end program main
