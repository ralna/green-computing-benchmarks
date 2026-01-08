program main
    use benchmark_types, only: BenchmarkContainer
    use benchmark_config, only: read_config
    use iso_fortran_env, only: real64, int64

    implicit none (external)

    class(BenchmarkContainer), allocatable :: benchmark_array(:)
    !! Array of benchmark types, each of which corresponds to a routine to benchmark

    character(len=64) :: blas_name
    !! Name of BLAS backend the benchmarks are being run with
    character(len=64) :: config_file
    !! Name of TOML config file
    integer :: i
    !! Loop counter

    if ( command_argument_count() == 0 ) then
        print *, "Please supply a config file to read"
        stop 1
    end if
    
    call get_command_argument(1, config_file)

    call read_config(config_file, benchmark_array)

    do i = 1, size(benchmark_array)
        call get_environment_variable("FLEXIBLAS", blas_name)
        
        call benchmark_array(i)%b%run(blas_name)
    end do

end program main
