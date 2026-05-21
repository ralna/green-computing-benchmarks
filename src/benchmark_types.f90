module benchmark_types
   use iso_fortran_env, only: int64, real64
   use tomlf, only: toml_table, toml_array, get_value
   implicit none (external)
   public :: write_header
   type, public, abstract :: Benchmark
      integer(int64) :: num_flops = 0.
      character(len=32) :: name

      integer, dimension(:), allocatable :: m_sizes
      integer, dimension(:), allocatable :: n_sizes
      integer, dimension(:), allocatable :: k_sizes

   contains
      procedure(run), deferred :: run
      procedure(call_benchmark), deferred :: call_benchmark
      procedure(init), deferred :: init
      procedure :: open_results_file
      procedure :: get_filename
      procedure :: time_benchmark
   end type Benchmark

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

      subroutine init(self, benchmark_table)
         import
         implicit none (external)
         class(Benchmark), intent(inout) :: self
         type(toml_table), pointer, intent(in) :: benchmark_table

      end subroutine init
   end interface

   interface write_header
      module procedure header_3_dim, header_2_dim, header_1_dim
   end interface

contains
   subroutine format_time_str(ts)
      character(len=*), intent(out) :: ts
      integer :: values(8)
      character(len=32) :: tmp
      call date_and_time(values=values)
      write(tmp, '(I0,A,I0,A,I0,A,I0,A,I0,A,I0)') &
         values(1),"-",values(2),"-",values(3),"_", &
         values(5),":",values(6),":",values(7)
      ts = trim(adjustl(tmp))
   end subroutine format_time_str

   character(len=256) function get_filename(self) result(filename)
      class(Benchmark), intent(inout) :: self
      character(len=80) :: time_str

      call format_time_str(time_str)

      filename = "results_"//trim(self%name)//"_"//trim(time_str)//".csv"
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

   subroutine open_results_file(self, iunit, file_exists)
      class(Benchmark), intent(inout) :: self
      integer, intent(out) :: iunit

      character(len = 256) :: filename
      logical, intent(out) :: file_exists

      filename = self%get_filename()

      inquire(file=filename, exist=file_exists)
      open(newunit=iunit, file=filename, position="append")
   end subroutine open_results_file


   subroutine read_array(table, arr_name, benchmark_name,  array)
      !! Read array in toml table called "name" into the provided array

      type(toml_table), pointer, intent(in) :: table
      !! toml table containing benchmark config

      character(len=*), intent(in) :: arr_name
      !! Name of array to read in

      character(len=*), intent(in) :: benchmark_name
      !! Name of benchmark config we are reading
      !! Needed for error message

      integer, allocatable, intent(out) ::  array(:)
      !! Array to populate

      type(toml_array), pointer :: toml_arr
      !! TOML array to hold size array information

      call get_value(table, arr_name, toml_arr)
      call get_value(toml_arr, array)

      if ( size(array) <= 0 ) then
         print *, "Missing ",  arr_name, " in ", trim(benchmark_name), " config"
         stop 1
      end if

   end subroutine read_array

   subroutine header_3_dim (iunit, dim1_name, dim1, dim2_name, dim2, dim3_name, dim3)
      character(len=*), intent(in) :: dim1_name
      !! Name associated with first dimension
      integer, intent(in), dimension(:) ::  dim1
      !! Array of sizes of first dimension

      character(len=*), intent(in) :: dim2_name
      !! Name associated with second dimension
      integer, intent(in), dimension(:) ::  dim2
      !! Array of sizes of second dimension

      character(len=*), intent(in) :: dim3_name
      !! Name associated with third dimension
      integer, intent(in), dimension(:) ::  dim3
      !! Array of sizes of third dimension

      integer, intent(in) :: iunit
      !! Unit to write to

      integer :: i, j, k
      !! Loop counter

      write(iunit, '(2A)', advance='no') dim1_name, ','

      do i = 1, size(dim1)
         do j = 1, size(dim2) * size(dim3)
            write(iunit, '(I6,A)', advance='no') dim1(i), ','
         end do
      end do
      write(iunit, '(A)') ''

      write(iunit, '(2A)', advance='no') dim2_name, ','

      do k = 1, size(dim1)
         do i = 1, size(dim2)
            do j = 1, size(dim3)
               write(iunit, '(I6,A)', advance='no') dim2(i), ','
            end do
         end do
      end do
      write(iunit, '(A)') ''

      write(iunit, '(2A)', advance='no') dim3_name, ','
      do j = 1, size(dim1) * size(dim2)
         do i = 1, size(dim3)
            write(iunit, '(I6,A)', advance='no') dim3(i), ','
         end do
      end do
      write(iunit, '(A)') ''
   end subroutine header_3_dim

   subroutine header_2_dim (iunit, dim1_name, dim1, dim2_name, dim2)
      character(len=*), intent(in) :: dim1_name
      !! Name associated with first dimension
      integer, intent(in), dimension(:) ::  dim1
      !! Array of sizes of first dimension

      character(len=*), intent(in) :: dim2_name
      !! Name associated with second dimension
      integer, intent(in), dimension(:) ::  dim2
      !! Array of sizes of second dimension

      integer, intent(in) :: iunit
      !! Unit to write to

      integer :: i, j
      !! Loop counter

      write(iunit, '(2A)', advance='no') dim1_name, ','

      do i = 1, size(dim1)
         do j = 1, size(dim2)
            write(iunit, '(I6,A)', advance='no') dim1(i), ','
         end do
      end do
      write(iunit, '(A)') ''

      write(iunit, '(2A)', advance='no') dim2_name, ','

      do i = 1, size(dim2)
         do j = 1, size(dim1)
            write(iunit, '(I6,A)', advance='no') dim2(i), ','
         end do
      end do
      write(iunit, '(A)') ''
   end subroutine header_2_dim

   subroutine header_1_dim (iunit, dim1_name, dim1)
      character(len=*), intent(in) :: dim1_name
      !! Name associated with first dimension
      integer, intent(in), dimension(:) ::  dim1
      !! Array of sizes of first dimension

      integer, intent(in) :: iunit
      !! Unit to write to

      integer :: i
      !! Loop counter

      write(iunit, '(2A)', advance='no') dim1_name, ','

      do i = 1, size(dim1)
         write(iunit, '(I6,A)', advance='no') dim1(i), ','
      end do

      write(iunit, '(A)') ''
   end subroutine header_1_dim
end module benchmark_types
