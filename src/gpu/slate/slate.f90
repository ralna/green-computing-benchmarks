module slate_benchmarks
    use benchmark_base, only: Benchmark
    use iso_fortran_env, only: int64, real64
    use slate_utils
    use mpi
    use slate
    implicit none (external)
    private

    type, public, extends(Benchmark) :: SLATE_MULT_DBenchmark
        integer(int64) :: m
        integer(int64) :: n
        integer(int64) :: k

        double precision :: alpha = 1.0
        double precision :: beta = 1.0

        type(c_ptr) :: A, B, C, opts

        contains
            procedure :: run => run_slate_mult_d
            procedure :: call_benchmark => call_slate_mult_d
            procedure, nopass :: get_filename => slate_mult_d_filename
            procedure, nopass :: write_headers => slate_mult_d_headers

    end type SLATE_MULT_DBenchmark

    type, public, extends(Benchmark) :: SLATE_MULT_SBenchmark
        integer(int64) :: m
        integer(int64) :: n
        integer(int64) :: k

        real :: alpha = 1.0
        real :: beta = 1.0

        type(c_ptr) :: A, B, C, opts

        contains
            procedure :: run => run_slate_mult_s
            procedure :: call_benchmark => call_slate_mult_s
            procedure, nopass :: get_filename => slate_mult_s_filename
            procedure, nopass :: write_headers => slate_mult_s_headers

    end type SLATE_MULT_SBenchmark

contains
    character(len=64) function slate_mult_d_filename() result(filename)
        filename = "results_SLATE_MULT_D.csv"
        return
    end function slate_mult_d_filename

    character(len=64) function slate_mult_s_filename() result(filename)
        filename = "results_SLATE_MULT_S.csv"
        return
    end function slate_mult_s_filename

    subroutine run_slate_mult_d(self, blas_name, iunit)
        class(SLATE_MULT_DBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        integer, intent(in) :: iunit

        integer :: i
        real(real64) :: avg_gflops

        integer(kind=c_int) :: p_grid, q_grid, mpi_size, ierr

        !Block size
        integer(int64) :: nb = 256

        call MPI_Comm_size( MPI_COMM_WORLD, mpi_size, ierr )
        call grid_size( mpi_size, p_grid, q_grid ) 

        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = 10, 15
            self%m = 2**i
            self%n = 2**i
            self%k = 2**i
            
            self%A = slate_Matrix_create_r64(self%m, self%k, nb, p_grid, q_grid, MPI_COMM_WORLD)
            self%B = slate_Matrix_create_r64(self%k, self%n, nb, p_grid, q_grid, MPI_COMM_WORLD)
            self%C = slate_Matrix_create_r64(self%m, self%n, nb, p_grid, q_grid, MPI_COMM_WORLD)

            call slate_Matrix_insertLocalTiles_r64(self%A)
            call slate_Matrix_insertLocalTiles_r64(self%B)
            call slate_Matrix_insertLocalTiles_r64(self%C)

            call random_Matrix_r64(self%A)
            call random_Matrix_r64(self%B)
            call random_Matrix_r64(self%C)

            self%num_flops = 2 * self%m * self%n * self%k

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','
        end do

        write(iunit, '(A)') ''
    end subroutine run_slate_mult_d

    subroutine run_slate_mult_s(self, blas_name, iunit)
        class(SLATE_MULT_SBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        integer, intent(in) :: iunit

        integer :: i
        real(real64) :: avg_gflops

        integer(kind=c_int) :: p_grid, q_grid, mpi_size, ierr

        !Block size
        integer(int64) :: nb = 256

        call MPI_Comm_size( MPI_COMM_WORLD, mpi_size, ierr )
        call grid_size( mpi_size, p_grid, q_grid ) 

        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = 10, 15
            self%m = 2**i
            self%n = 2**i
            self%k = 2**i

            self%A = slate_Matrix_create_r32(self%m, self%k, nb, p_grid, q_grid, MPI_COMM_WORLD)
            self%B = slate_Matrix_create_r32(self%k, self%n, nb, p_grid, q_grid, MPI_COMM_WORLD)
            self%C = slate_Matrix_create_r32(self%m, self%n, nb, p_grid, q_grid, MPI_COMM_WORLD)

            call slate_Matrix_insertLocalTiles_r32(self%A)
            call slate_Matrix_insertLocalTiles_r32(self%B)
            call slate_Matrix_insertLocalTiles_r32(self%C)

            call random_Matrix_r32(self%A)
            call random_Matrix_r32(self%B)
            call random_Matrix_r32(self%C)

            self%num_flops = 2 * self%m * self%n * self%k

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','
        end do

        write(iunit, '(A)') ''
    end subroutine run_slate_mult_s

    subroutine call_slate_mult_d(self)
        class(SLATE_MULT_DBenchmark), intent(inout) :: self
        call slate_multiply_r64(&
            self%alpha,&
            self%A,&
            self%B,&
            self%beta,&
            self%C,&
            self%opts&
        )
    end subroutine call_slate_mult_d

    subroutine call_slate_mult_s(self)
        class(SLATE_MULT_SBenchmark), intent(inout) :: self
        call slate_multiply_r32(&
            self%alpha,&
            self%A,&
            self%B,&
            self%beta,&
            self%C,&
            self%opts&
        )
    end subroutine call_slate_mult_s

    subroutine slate_mult_d_headers(iunit)
        integer, intent(in) :: iunit
        integer :: i

        write(iunit, '(A)') 'Average performance of SLATE_MULT_D (GFLOPS/s)'
        write(iunit, '(A)') ',Matrix size,'
        write(iunit, '(A)', advance='no') 'BLAS backend,'

        do i = 10, 15
            write(iunit, '(I6,A)', advance='no') 2**i, ','
        end do

        write(iunit, '(A)') ''
    end subroutine slate_mult_d_headers

    subroutine slate_mult_s_headers(iunit)
        integer, intent(in) :: iunit
        integer :: i

        write(iunit, '(A)') 'Average performance of SLATE_MULT_S (GFLOPS/s)'
        write(iunit, '(A)') ',Matrix size,'
        write(iunit, '(A)', advance='no') 'BLAS backend,'

        do i = 10, 15
            write(iunit, '(I6,A)', advance='no') 2**i, ','
        end do

        write(iunit, '(A)') ''
    end subroutine slate_mult_s_headers


end module slate_benchmarks