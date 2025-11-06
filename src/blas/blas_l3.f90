module blas_l3_benchmarks
   use benchmark_types, only: Benchmark, L3Benchmark
   use blas_interfaces, only: dgemm, sgemm, dsyrk, ssyrk, dsyr2k, ssyr2k
   use iso_fortran_env, only: int64, real64
   implicit none (external)
   private

   type, public, extends(L3Benchmark) :: DGEMMBenchmark
      double precision :: alpha = 1.0
      double precision :: beta = 1.0
   contains
      procedure :: call_benchmark => call_dgemm
   end type DGEMMBenchmark
contains

   subroutine call_dgemm(self)
      class(DGEMMBenchmark), intent(inout) :: self
      self%num_flops = 2 * self%m * self%n * self%k
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
   end subroutine call_dgemm
end module blas_l3_benchmarks
