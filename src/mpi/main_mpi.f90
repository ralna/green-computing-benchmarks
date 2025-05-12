program main
    use benchmark_base, only: Benchmark, BenchmarkContainer
    use pblas_l2_benchmarks, only: PDGEMVBenchmark
    use iso_fortran_env, only: real64, int64

    implicit none (external)

    integer :: i

    class(Benchmark), allocatable :: b

    class(BenchmarkContainer), allocatable :: benchmark_array(:)

    character(len=64) :: filename
    character(len=16) :: blas_name
    integer :: iunit
    logical :: file_exists

    allocate(benchmark_array(1))
    allocate(PDGEMVBenchmark::benchmark_array(1)%b)

    do i = 1, size(benchmark_array)
        b = benchmark_array(i)%b

        call get_environment_variable("FLEXIBLAS", blas_name)

        filename = b%get_filename()

        inquire(file=filename, exist=file_exists)
        open(newunit=iunit, file=filename, position="append")

        if (.not. file_exists) then
            call b%write_headers(iunit)
        end if

        call b%run(blas_name, iunit)
        close(iunit)

    end do

end program main
