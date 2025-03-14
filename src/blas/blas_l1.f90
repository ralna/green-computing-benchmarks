module blas_l1_benchmarks
    use iso_fortran_env, only: int64
    use benchmark_base
    implicit none

    type, public, extends(Benchmark) :: DASUMBenchmark
        integer(int64) :: n = 1000

        double precision, dimension(:), allocatable :: x


        contains
            procedure :: setup => setup_dasum
            procedure :: run => run_dasum
        
    end type DASUMBenchmark
    
contains
    subroutine setup_dasum(self)
        class(DASUMBenchmark), intent(inout) :: self
        
        allocate(self%x(self%n))
                
        call random_number(self%x)
    
        self%num_flops = self%n
        self%name = "DASUM"
    
    end subroutine setup_dasum
    
    subroutine run_dasum(self)
        class(DASUMBenchmark), intent(inout) :: self     
        call dasum(self%n, self%x, 1)

    end subroutine run_dasum
    
end module blas_l1_benchmarks