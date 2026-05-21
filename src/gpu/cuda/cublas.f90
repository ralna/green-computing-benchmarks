module cublas_benchmarks
   use benchmark_types, only: Benchmark, read_array, write_header
   use iso_fortran_env, only: int64, real64, int32
   use iso_c_binding, only: c_ptr, c_null_ptr, c_size_t, c_int, c_long, c_double
   use cuda_interfaces
   use tomlf, only: toml_table
   implicit none (external)
   private

   type, public, extends(Benchmark) :: CUBLASDGEMMBenchmark
      integer(c_long) :: m = 1
      integer(c_long) :: n = 1
      integer(c_long) :: k = 1

      real(c_double) :: alpha = 1.0
      real(c_double) :: beta = 1.0

      ! Host arrays
      real(c_double), dimension(:,:), pointer :: A => null()
      real(c_double), dimension(:,:), pointer :: B => null()
      real(c_double), dimension(:,:), pointer :: C => null()

      ! Device arrays
      type(c_ptr) :: A_d = c_null_ptr
      type(c_ptr) :: B_d = c_null_ptr
      type(c_ptr) :: C_d = c_null_ptr

      ! cuBLAS handle
      type(c_ptr) :: handle = c_null_ptr

   contains
      procedure :: run => run_cublas_dgemm
      procedure :: call_benchmark => call_cublas_dgemm
      procedure :: init => init_cublas_dgemm
   end type CUBLASDGEMMBenchmark

contains

   !!!!!!!!!!!!!!!!
   ! CUBLAS DGEMM !
   !!!!!!!!!!!!!!!!
   subroutine init_cublas_dgemm(self, benchmark_table)
      class(CUBLASDGEMMBenchmark), intent(inout) :: self
      type(toml_table), pointer, intent(in) :: benchmark_table

      call read_array(benchmark_table, "m-sizes", self%name, self%m_sizes)
      call read_array(benchmark_table, "n-sizes", self%name, self%n_sizes)
      call read_array(benchmark_table, "k-sizes", self%name, self%k_sizes)
   end subroutine init_cublas_dgemm

   subroutine run_cublas_dgemm(self, blas_name)
      class(CUBLASDGEMMBenchmark), intent(inout) :: self
      character(len=16), intent(in) :: blas_name

      ! status codes
      integer(c_int) :: istat

      integer :: i, j, k, iunit
      real(real64) :: avg_gflops
      logical :: file_exists

      ! init CUBLAS handle
      self%handle = c_null_ptr
      istat = cublasCreate(self%handle)

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

               self%num_flops = 2 * self%m * self%n * self%k

               allocate(self%A(self%m , self%k))
               allocate(self%B(self%k , self%n))
               allocate(self%C(self%m , self%n))

               call random_number(self%A)
               call random_number(self%B)
               call random_number(self%C)

               ! Allocate device memory
               istat = cudaMalloc(self%A_d,&
                  int(self%m*self%k, c_size_t)*c_sizeof(self%A(1,1)))
               istat = cudaMalloc(self%B_d,&
                  int(self%k*self%n, c_size_t)*c_sizeof(self%B(1,1)))
               istat = cudaMalloc(self%C_d,&
                  int(self%m*self%n, c_size_t)*c_sizeof(self%C(1,1)))

               ! Copy to device
               istat = cudaMemcpy(self%A_d,&
                  c_loc(self%A(1,1)), int(self%m*self%k, c_size_t)*c_sizeof(self%A(1,1)), cudaMemcpyHostToDevice)

               istat = cudaMemcpy(self%B_d,&
                  c_loc(self%B(1,1)), int(self%k*self%n, c_size_t)*c_sizeof(self%B(1,1)), cudaMemcpyHostToDevice)

               istat = cudaMemcpy(self%C_d,&
                  c_loc(self%C(1,1)), int(self%m*self%n, c_size_t)*c_sizeof(self%C(1,1)), cudaMemcpyHostToDevice)

               print *, istat

               write(iunit, '(F13.7,A)', advance='no') avg_gflops, ','

               istat = cudaFree(self%A_d)
               istat = cudaFree(self%B_d)
               istat = cudaFree(self%C_d)

               deallocate(self%A)
               deallocate(self%B)
               deallocate(self%C)
            end do
         end do
      end do

      write(iunit, '(A)') ''

      close(iunit)
   end subroutine run_cublas_dgemm

   subroutine call_cublas_dgemm(self)
      class(CUBLASDGEMMBenchmark), intent(inout) :: self
      integer :: stat

      real(c_double), target :: alpha_t, beta_t

      alpha_t = self%alpha
      beta_t = self%beta


      stat = cublasDgemm(&
         self%handle,&
         CUBLAS_OP_N,&
         CUBLAS_OP_N,&
         self%m,&
         self%n,&
         self%k,&
         c_loc(alpha_t),&
         self%A_d,&
         self%m,&
         self%B_d,&
         self%k,&
         c_loc(beta_t),&
         self%C_d,&
         self%m&
         )

      stat = cudaDeviceSynchronize()
   end subroutine call_cublas_dgemm
end module cublas_benchmarks
