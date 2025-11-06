module blas_l2_benchmarks
   use benchmark_types, only: L2Benchmark
   use blas_interfaces, only: dgemv, sgemv
   use iso_fortran_env, only: int64, real64
   implicit none (external)
   private

   type, public, extends(L2Benchmark) :: DGEMVBenchmark
      double precision :: alpha = 1.0
      double precision :: beta = 1.0
   contains
      procedure :: call_benchmark => call_dgemv
   end type DGEMVBenchmark

contains
   subroutine call_dgemv(self)
      class(DGEMVBenchmark), intent(inout) :: self
      self%num_flops = 2 * self%m * self%n

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

end module blas_l2_benchmarks
