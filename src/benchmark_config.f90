module benchmark_config
   use iso_fortran_env, only: int64, real64
   use misc_benchmarks, only: DummyBenchmark, NaiveMatmulBenchmark
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
      !! Read in a TOML config file, initialise the benchmarks that it
      !! specifies and store them in the supplied array

      class(BenchmarkContainer), intent(out), allocatable :: benchmarks(:)

      type(toml_table), allocatable :: config_table
      !! TOML table of all benchmarks
      type(toml_array), pointer :: toml_arr
      !! Array of benchmark config tables
      type(toml_error), allocatable :: error
      !! Error type for parsing config file
      type(toml_table), pointer :: benchmark_table
      !! Table of an individual benchmark's config

      integer :: in_unit
      !! Unit to read config file from
      integer :: i
      !! Loop counter

      !Read file
      open(file="example.toml", newunit=in_unit)

      config_table = toml_table()

      call toml_parse(config_table, in_unit, error)
      close(in_unit)
      if (allocated(error)) then
         print '(a)', "Error: "//error%message
         stop 1
      end if

      !Get global config options
      call get_value(config_table, "num_repeats", num_repeats)

      ! Loop over list of benchmark tables
      call get_value(config_table, "benchmarks", toml_arr)
      allocate(benchmarks(len(toml_arr)))

      do i = 1, size(benchmarks)
         call get_value(toml_arr, i, benchmark_table)
         ! Set up benchmark object according to config
         call init_benchmark(cur_benchmark, benchmarks(i))
      end do
   end subroutine read_config

   subroutine init_benchmark(benchmark_table, benchmark)
      !! Initialise a benchmark type according to the given TOML config

      type(toml_table), intent(inout) :: benchmark_table
      !! TOML table containing benchmark parameters
      character(len=:), allocatable :: name
      !! Name of the benchmark
      type(toml_array), pointer :: toml_arr
      !! TOML array to hold size array information
      integer, allocatable :: m_sizes(:), n_sizes(:), k_sizes(:)
      !! Arrays to specify the size of the benchmark problems
      class(BenchmarkContainer) :: benchmark
      !! Benchmark type which we will initialise to the type of
      !! the specified routine

      !Get name
      call get_value(benchmark_table, "name", name)

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
      call get_value(toml_arr, m_sizes)
      benchmark%b%m_sizes = m_sizes


      !Always need m
      if ( size(m_sizes) <= 0 ) then
         print *, "Missing m-sizes in ", name, " config"
         stop 1
      end if

      select type(b => benchmark%b)
       class is (L3Benchmark)
         call get_value(benchmark_table, "n-sizes", toml_arr)
         call get_value(toml_arr, n_sizes)
         benchmark%b%n_sizes = n_sizes

         call get_value(benchmark_table, "k-sizes", toml_arr)
         call get_value(toml_arr, k_sizes)
         benchmark%b%k_sizes = k_sizes

         if ( size(n_sizes) <= 0 ) then
            print *, "Missing n-sizes in ", name, " config"
            stop 1
         else if ( size(k_sizes) <= 0 ) then
            print *, "Missing k-sizes in ", name, " config"
            stop 1
         end if
       class is (L2Benchmark)
         call get_value(benchmark_table, "n-sizes", toml_arr)
         call get_value(toml_arr, n_sizes)
         benchmark%b%n_sizes = n_sizes

         if ( size(n_sizes) <= 0 ) then
            print *, "Missing n-sizes in ", name, " config"
            stop 1
         end if
      end select
   end subroutine init_benchmark

   subroutine select_benchmark(name, benchmark)
      !! Match given name to derived type for a routine
      !! and allocate space for that type

      character(len=:), allocatable, intent(in) :: name
      !! Benchmark name
      class(BenchmarkContainer), intent(inout) :: benchmark
      !! Generic container type that will be allocated as a 
      !! derived type specific to a routine

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

       case ("NAIVE")
         allocate(NaiveMatmulBenchmark::benchmark%b)
         benchmark%b = NaiveMatmulBenchmark(name="Naive")

       case default
         allocate(DummyBenchmark::benchmark%b)
         benchmark%b = DummyBenchmark(name="failed")
      end select

      return
   end subroutine select_benchmark
end module benchmark_config
