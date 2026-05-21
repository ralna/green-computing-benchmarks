module hipblas_benchmarks
   use benchmark_types, only: Benchmark, read_array, write_header
   use iso_fortran_env, only: int64, real64, int32
   use iso_c_binding, only: c_ptr, c_null_ptr
   use tomlf, only: toml_table
   use hipfort
   use hipfort_hipblas
   implicit none

   type, public, extends(Benchmark) :: HIPBLASDGEMMBenchmark
      integer(int32) :: m = 1
      integer(int32) :: n = 1
      integer(int32) :: k = 1

      real(real64) :: alpha = 1.0
      real(real64) :: beta = 1.0

      ! Host arrays
      real(real64), allocatable, dimension(:,:) :: A
      real(real64), allocatable, dimension(:,:) :: B
      real(real64), allocatable, dimension(:,:) :: C

      ! Device arrays
      real(real64), pointer, dimension(:,:) :: A_d => null()
      real(real64), pointer, dimension(:,:) :: B_d => null()
      real(real64), pointer, dimension(:,:) :: C_d => null()

      ! hipBLAS handle
      type(c_ptr) :: handle = c_null_ptr

   contains
      procedure :: run => run_hipblas_dgemm
      procedure :: call_benchmark => call_hipblas_dgemm
      procedure :: init => init_hipblas_dgemm
   end type HIPBLASDGEMMBenchmark

contains
   !!!!!!!!!!!!!!!!!
   ! HIPBLAS DGEMM !
   !!!!!!!!!!!!!!!!!
   subroutine init_hipblas_dgemm(self, benchmark_table)
      class(HIPBLASDGEMMBenchmark), intent(inout) :: self
      type(toml_table), pointer, intent(in) :: benchmark_table

      call read_array(benchmark_table, "m-sizes", self%name, self%m_sizes)
      call read_array(benchmark_table, "n-sizes", self%name, self%n_sizes)
      call read_array(benchmark_table, "k-sizes", self%name, self%k_sizes)
   end subroutine init_hipblas_dgemm

   subroutine run_hipblas_dgemm(self, blas_name)
      class(HIPBLASDGEMMBenchmark), intent(inout) :: self
      character(len=16), intent(in) :: blas_name

      ! status codes
      integer(c_int) :: istat

      integer :: i, j, k, iunit
      real(real64) :: avg_gflops
      logical :: file_exists

      ! Set handle
      self%handle = c_null_ptr
      istat = hipblasCreate(self%handle)
  

      call self%open_results_file(iunit, file_exists)

      if (.not. file_exists) then
         call write_header( iunit,&
            "m", self%m_sizes,&
            "n", self%n_sizes,&
            "k", self%k_sizes)
      end if

      write(iunit, '(A)', advance='no') 'GFLOPS/s,'

      do i = 1, size(self%m_sizes)
         do j = 1, size(self%n_sizes)
            do k = 1, size(self%k_sizes)
               self%m = self%m_sizes(i)
               self%n = self%n_sizes(j)
               self%k = self%k_sizes(k)

               self%num_flops = 2_int64 * int8(self%m) * int8(self%n) * int8(self%k)

               allocate(self%A(self%m , self%k))
               allocate(self%B(self%k , self%n))
               allocate(self%C(self%m , self%n))

               call random_number(self%A)
               call random_number(self%B)
               call random_number(self%C)

               ! Allocate device memory and copy from host
               istat = hipMalloc(self%A_d, source=self%A)
               istat = hipMalloc(self%B_d, source=self%B)
               istat = hipMalloc(self%C_d, source=self%C)

               avg_gflops = self%time_benchmark(100)

               write(iunit, '(F13.7,A)', advance='no') avg_gflops, ','

               istat = hipFree(self%A_d)
               istat = hipFree(self%B_d)
               istat = hipFree(self%C_d)

               deallocate(self%A)
               deallocate(self%B)
               deallocate(self%C)
            end do
         end do
      end do

      write(iunit, '(A)') ''

      close(iunit)
   end subroutine run_hipblas_dgemm

   subroutine call_hipblas_dgemm(self)
      class(HIPBLASDGEMMBenchmark), intent(inout) :: self
      integer :: stat

      stat = hipblasdgemm(&
         self%handle,&
         HIPBLAS_OP_N,&
         HIPBLAS_OP_N,&
         self%m,&
         self%n,&
         self%k,&
         self%alpha,&
         self%A_d,&
         self%m,&
         self%B_d,&
         self%k,&
         self%beta,&
         self%C_d,&
         self%m&
         )

      stat = hipDeviceSynchronize()
   end subroutine call_hipblas_dgemm
end module hipblas_benchmarks
