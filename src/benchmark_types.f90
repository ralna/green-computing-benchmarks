module benchmark_types
   use iso_fortran_env, only: int64, real64
   implicit none (external)
   type, public, abstract :: Benchmark
      integer(int64) :: num_flops = 0.
      character(len=32) :: name

      integer, dimension(:), allocatable :: m_sizes
      integer, dimension(:), allocatable :: n_sizes
      integer, dimension(:), allocatable :: k_sizes

   contains
      procedure(run), deferred :: run
      procedure(call_benchmark), deferred :: call_benchmark
      procedure :: open_results_file
      procedure :: get_filename
      procedure :: time_benchmark
   end type Benchmark

   type, public, abstract, extends(Benchmark) :: L3Benchmark
      integer(int64) :: m = 1
      integer(int64) :: n = 1
      integer(int64) :: k = 1

      double precision, dimension(:,:), allocatable :: A
      double precision, dimension(:,:), allocatable :: B
      double precision, dimension(:,:), allocatable :: C
   contains
      procedure :: run => run_l3

   end type L3Benchmark

   type, public, abstract, extends(Benchmark) :: L2Benchmark
      integer(int64) :: m = 1
      integer(int64) :: n = 1

      double precision, dimension(:,:), allocatable :: A
      double precision, dimension(:), allocatable :: x
      double precision, dimension(:), allocatable :: y

   contains
      procedure :: run => run_l2

   end type L2Benchmark

   type, public, abstract, extends(Benchmark) :: L1Benchmark
      integer(int64) :: m = 1

      double precision, dimension(:), allocatable :: X
      double precision, dimension(:), allocatable :: Y

   contains
      procedure :: run => run_l1

   end type L1Benchmark

   type, public :: BenchmarkContainer
      class(Benchmark), allocatable :: b
   end type BenchmarkContainer

   abstract interface
      subroutine run(self, blas_name)
         import
         implicit none (external)
         class(Benchmark), intent(inout) :: self
         character(len=16), intent(in) :: blas_name
      end subroutine run

      subroutine call_benchmark(self)
         import
         implicit none (external)
         class(Benchmark), intent(inout) :: self
      end subroutine call_benchmark
   end interface

contains
   character(len=64) function get_filename(self) result(filename)
      class(Benchmark), intent(inout) :: self

      filename = "results_"//trim(self%name)//".csv"
      return
   end function get_filename

   function time_benchmark(self, n_iters) result(max_flops)
      class(Benchmark), intent(inout) :: self
      integer, intent(in) :: n_iters

      integer(int64) :: start_count, end_count
      integer(int64) :: count_rate, count_max

      real(real64) :: elapsed_time

      real(real64) :: max_flops, flops

      integer :: i

      max_flops = 0
      do i = 1, n_iters
         call system_clock(start_count, count_rate, count_max)
         call self%call_benchmark()
         call system_clock(end_count, count_rate, count_max)

         !Time (s)
         elapsed_time = real(end_count - start_count) / real(count_rate)

         !FLOPS/s
         flops = real(self%num_flops) / (elapsed_time)

         max_flops = max(max_flops, flops)

      end do

      max_flops = max_flops / (1000.0**3)
      return
   end function time_benchmark

   subroutine open_results_file(self, iunit)
      class(Benchmark), intent(inout) :: self
      integer, intent(out) :: iunit

      integer :: i, j, k
      character(len = 64) :: filename
      logical :: file_exists

      filename = self%get_filename()

      inquire(file=filename, exist=file_exists)
      open(newunit=iunit, file=filename, position="append")

      if (.not. file_exists) then
         select type (self)
          class is (L3Benchmark)
            write(iunit, '(A)', advance='no') 'm,'

            do i = 1, size(self%m_sizes)
               do j = 1, 9
                  write(iunit, '(I6,A)', advance='no') self%m_sizes(i), ','
               end do
            end do
            write(iunit, '(A)') ''

            write(iunit, '(A)', advance='no') 'n,'

            do k = 1,3
               do i = 1, size(self%n_sizes)
                  do j = 1, 3
                     write(iunit, '(I6,A)', advance='no') self%n_sizes(i), ','
                  end do
               end do
            end do
            write(iunit, '(A)') ''

            write(iunit, '(A)', advance='no') 'k,'
            do j = 1, 9
               do i = 1, size(self%k_sizes)
                  write(iunit, '(I6,A)', advance='no') self%n_sizes(i), ','
               end do
            end do
            write(iunit, '(A)') ''

          class is (L2Benchmark)
            write(iunit, '(A)', advance='no') 'm,'

            do i = 1, size(self%m_sizes)
               do j = 1, 3
                  write(iunit, '(I6,A)', advance='no') self%m_sizes(i), ','
               end do
            end do
            write(iunit, '(A)') ''

            write(iunit, '(A)', advance='no') 'n,'

            do j = 1, 3
               do i = 1, size(self%n_sizes)
                  write(iunit, '(I6,A)', advance='no') self%n_sizes(i), ','
               end do
            end do
            write(iunit, '(A)') ''

          class is (L1Benchmark)
            write(iunit, '(A)', advance='no') 'm,'
            do i = 1, size(self%m_sizes)
               write(iunit, '(I6,A)', advance='no') self%m_sizes(i), ','
            end do
            write(iunit, '(A)') ''
         end select
      end if
   end subroutine open_results_file

   subroutine run_l3(self, blas_name)
      class(L3Benchmark), intent(inout) :: self
      character(len=16), intent(in) :: blas_name

      integer :: i, j, k, iunit
      real(real64) :: avg_gflops

      call self%open_results_file(iunit)
      write(iunit, '(2A)', advance='no') blas_name, ','

      do i = 1, size(self%m_sizes)
         do j = 1, size(self%n_sizes)
            do k = 1, size(self%k_sizes)
               self%m = self%m_sizes(i)
               self%n = self%n_sizes(j)
               self%k = self%k_sizes(k)

               allocate(self%A(self%m , self%k))
               allocate(self%B(self%k , self%n))
               allocate(self%C(self%m , self%n))

               call random_number(self%A)
               call random_number(self%B)
               call random_number(self%C)

               avg_gflops = self%time_benchmark(100)

               write(iunit, '(F13.7,A)', advance='no') avg_gflops, ','

               deallocate(self%A)
               deallocate(self%B)
               deallocate(self%C)
            end do
         end do
      end do

      write(iunit, '(A)') ''

      close(iunit)

   end subroutine run_l3

   subroutine run_l2(self, blas_name)
      class(L2Benchmark), intent(inout) :: self
      character(len=16), intent(in) :: blas_name
      integer :: i, j, iunit
      real(real64) :: avg_gflops

      call self%open_results_file(iunit)
      write(iunit, '(2A)', advance='no') blas_name, ','

      do i = 1, size(self%m_sizes)
         do j = 1, size(self%n_sizes)
            self%m = self%m_sizes(i)
            self%n = self%n_sizes(j)

            allocate(self%A(self%m , self%n))
            allocate(self%x(self%n))
            allocate(self%y(self%m))

            call random_number(self%A)
            call random_number(self%x)
            call random_number(self%y)

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F13.7,A)', advance='no') avg_gflops, ','

            deallocate(self%A)
            deallocate(self%x)
            deallocate(self%y)
         end do
      end do

      write(iunit, '(A)') ''

      close(iunit)

   end subroutine run_l2

   subroutine run_l1(self, blas_name)
      class(L1Benchmark), intent(inout) :: self
      character(len=16), intent(in) :: blas_name

      integer :: i, iunit
      real(real64) :: avg_gflops

      call self%open_results_file(iunit)
      write(iunit, '(2A)', advance='no') blas_name, ','

      do i = 1, size(self%m_sizes)
         self%m = self%m_sizes(i)

         allocate(self%x(self%m))
         allocate(self%y(self%m))

         call random_number(self%x)
         call random_number(self%y)

         self%num_flops = self%m

         avg_gflops = self%time_benchmark(100)

         write(iunit, '(F13.7,A)', advance='no') avg_gflops, ','

         deallocate(self%x)
         deallocate(self%y)
      end do

      write(iunit, '(A)') ''

      close(iunit)
   end subroutine run_l1

end module benchmark_types
