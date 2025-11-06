module benchmark_config
   use iso_fortran_env, only: int64, real64
   use misc_benchmarks, only: DummyBenchmark
   use blas_l3_benchmarks, only: DGEMMBenchmark
   use blas_l2_benchmarks, only: DGEMVBenchmark
   use blas_l1_benchmarks, only: DAXPYBenchmark
   use benchmark_types, only: Benchmark, BenchmarkContainer, L1Benchmark, L2Benchmark, L3Benchmark
   use tomlf, only : toml_table, toml_array, toml_parse, toml_error, get_value, len
   implicit none (external)
   public read_config
   private

contains

   subroutine read_config(benchmarks)
      integer :: num_repeats
      integer, allocatable :: sizes(:)

      class(BenchmarkContainer), intent(out), allocatable :: benchmarks(:)

      type(toml_table), allocatable :: table
      type(toml_array), pointer :: array
      type(toml_error), allocatable :: error
      type(toml_table), pointer :: cur_benchmark

      integer :: in_unit, i

      !Read file
      open(file="example.toml", newunit=in_unit)

      table = toml_table()

      call toml_parse(table, in_unit, error)
      close(in_unit)
      if (allocated(error)) then
         print '(a)', "Error: "//error%message
         stop 1
      end if

      !Get global config options
      call get_value(table, "num_repeats", num_repeats)

      ! Loop over list of benchmark tables
      call get_value(table, "benchmarks", array)
      allocate(benchmarks(len(array)))

      do i = 1, size(benchmarks)
         call get_value(array, i, cur_benchmark)
         ! Set up benchmark object according to config
         call init_benchmark(cur_benchmark, benchmarks(i))
      end do
   end subroutine read_config

   subroutine init_benchmark(benchmark_table, benchmark)
      type(toml_table), intent(inout) :: benchmark_table
      character(len=:), allocatable :: name, combination
      type(toml_array), pointer :: toml_arr
      integer, allocatable :: m_sizes(:), n_sizes(:), k_sizes(:)
      class(BenchmarkContainer) :: benchmark

      !Get name
      call get_value(benchmark_table, "name", name)
      print *, name

      !Create benchmark of correct derived type
      call select_benchmark(name, benchmark)

      select type(b => benchmark%b)
       class is (DummyBenchmark)
         print *, "WARNING: Failed to create benchmark for ", name, ": Invalid name"
         return
      end select

      !sizes
      !check that number of size arrays matches function
      !ignore any extra arrays provided

      !check for m n and k
      call get_value(benchmark_table, "m-sizes", toml_arr)

      !Always need m
      if ( associated(toml_arr) ) then
         call get_value(toml_arr, m_sizes)
         benchmark%b%m_sizes = m_sizes
      else
         print *, "Missing m-sizes in ", name, " config"
         stop 1
      end if

      call get_value(benchmark_table, "n-sizes", toml_arr)
      if ( associated(toml_arr) ) then
         select type(b => benchmark%b)
          class is (L3Benchmark)
            call get_value(toml_arr, n_sizes)
            benchmark%b%n_sizes = n_sizes

          class is (L2Benchmark)
            call get_value(toml_arr, n_sizes)
            benchmark%b%n_sizes = n_sizes

         end select
      else
         print *, "Missing n-sizes in ", name, " config"
         stop 1
      end if

      call get_value(benchmark_table, "k-sizes", toml_arr)
      if ( associated(toml_arr) ) then
         select type(b => benchmark%b)
          class is (L3Benchmark)
            call get_value(toml_arr, k_sizes)
            benchmark%b%k_sizes = k_sizes

         end select
      else
         print *, "Missing k-sizes in ", name, " config"
         stop 1
      end if
   end subroutine init_benchmark

   subroutine select_benchmark(name, benchmark)
      character(len=:), allocatable, intent(in) :: name
      class(BenchmarkContainer), intent(inout) :: benchmark

      select case (name)
       case ("DGEMM")
         allocate(DGEMMBenchmark::benchmark%b)
         benchmark%b = DGEMMBenchmark(name="DGEMM")

       case ("DGEMV")
         allocate(DGEMVBenchmark::benchmark%b)
         benchmark%b = DGEMVBenchmark(name="DGEMV")

       case ("DAXPY")
         allocate(DAXPYBenchmark::benchmark%b)
         benchmark%b = DAXPYBenchmark(name="DAXPY")
   
       case default
         allocate(DummyBenchmark::benchmark%b)
         benchmark%b = DummyBenchmark(name="failed")
      end select

      return
   end subroutine select_benchmark
end module benchmark_config
