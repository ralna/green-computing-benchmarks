module misc_benchmarks
   use benchmark_types, only: Benchmark, L3Benchmark
   use iso_fortran_env, only: int64, real64
   implicit none (external)
   private

   type, public, extends(L3Benchmark) :: NaiveMatmulBenchmark
      double precision :: alpha = 1.0
      double precision :: beta = 1.0

   contains
      procedure :: call_benchmark => call_naive_matmul
   end type NaiveMatmulBenchmark

   type, public, extends(Benchmark) :: DummyBenchmark
   contains
      procedure :: run => dummy_run
      procedure :: call_benchmark => dummy_call
   end type DummyBenchmark

contains
   subroutine call_naive_matmul(self)
      class(NaiveMatmulBenchmark), intent(inout) :: self
      self%num_flops = 2 * self%m * self%n * self%k
      call naive_matmul(&
         self%m,&
         self%n,&
         self%k,&
         self%alpha,&
         self%beta,&
         self%A,&
         self%B,&
         self%C&
         )
   end subroutine call_naive_matmul

   subroutine naive_matmul(m, n, k, alpha, beta, A, B, C)
      integer(int64), intent(in) :: m, n, k
      double precision, intent(in) :: alpha, beta
      double precision, dimension(:,:), intent(in) ::  A, B
      double precision, dimension(:,:), intent(inout) ::  C

      integer(int64) x, y, z

      do x = 1, m
         do y = 1, k
            do z = 1, n
               C(x, z) = beta * C(x, z) + alpha * A(x, y) * B(y, z)
            end do
         end do
      end do

   end subroutine naive_matmul

   subroutine dummy_run(self, blas_name)
      class(DummyBenchmark), intent(inout) :: self
      character(len=16), intent(in) :: blas_name
   end subroutine dummy_run

   subroutine dummy_call(self)
      class(DummyBenchmark), intent(inout) :: self
   end subroutine dummy_call
end module misc_benchmarks
