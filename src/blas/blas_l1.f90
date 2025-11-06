module blas_l1_benchmarks
    use benchmark_types, only: L1Benchmark
    use blas_interfaces, only: dasum, sasum, daxpy
    use iso_fortran_env, only: int64, real64
    implicit none (external)
    private

    type, public, extends(L1Benchmark) :: DAXPYBenchmark
        double precision :: alpha = 1.
        
        contains
            procedure :: call_benchmark => call_daxpy
    end type DAXPYBenchmark

contains
   
    subroutine call_daxpy(self)
        class(DAXPYBenchmark), intent(inout) :: self
        self%num_flops = self%m
        call daxpy(self%m, self%alpha, self%x, 1, self%y, 1)
    end subroutine call_daxpy


end module blas_l1_benchmarks