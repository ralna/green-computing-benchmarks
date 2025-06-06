module blas_l3_benchmarks
    use benchmark_base, only: Benchmark
    use blas_interfaces, only: dgemm, sgemm, dsyrk, ssyrk, dsyr2k, ssyr2k
    use iso_fortran_env, only: int64, real64
    implicit none (external)
    private

    type, public, extends(Benchmark) :: DGEMMBenchmark
        integer(int64) :: m
        integer(int64) :: n
        integer(int64) :: k

        double precision :: alpha = 1.0
        double precision :: beta = 1.0

        double precision, dimension(:,:), allocatable :: A
        double precision, dimension(:,:), allocatable :: B
        double precision, dimension(:,:), allocatable :: C

        contains
            procedure :: run => run_dgemm
            procedure :: call_benchmark => call_dgemm
    end type DGEMMBenchmark

    type, public, extends(Benchmark) :: SGEMMBenchmark
        integer(int64) :: m
        integer(int64) :: n
        integer(int64) :: k

        real :: alpha = 1.0
        real :: beta = 1.0

        real, dimension(:,:), allocatable :: A
        real, dimension(:,:), allocatable :: B
        real, dimension(:,:), allocatable :: C

        contains
            procedure :: run => run_sgemm
            procedure :: call_benchmark => call_sgemm
    end type SGEMMBenchmark

    type, public, extends(Benchmark) :: DSYRKBenchmark
        integer(int64) :: n
        integer(int64) :: k

        double precision :: alpha = 1.0
        double precision :: beta = 0.0

        double precision, dimension(:,:), allocatable :: A
        double precision, dimension(:,:), allocatable :: C

        contains
            procedure :: run => run_dsyrk
            procedure :: call_benchmark => call_dsyrk
    end type DSYRKBenchmark 

    type, public, extends(Benchmark) :: SSYRKBenchmark
        integer(int64) :: n
        integer(int64) :: k

        real :: alpha = 1.0
        real :: beta = 0.0

        real, dimension(:,:), allocatable :: A
        real, dimension(:,:), allocatable :: C

        contains
            procedure :: run => run_ssyrk
            procedure :: call_benchmark => call_ssyrk
    end type SSYRKBenchmark 

    type, public, extends(Benchmark) :: DSYR2KBenchmark
        integer(int64) :: n
        integer(int64) :: k

        double precision :: alpha = 1.0
        double precision :: beta = 0.0

        double precision, dimension(:,:), allocatable :: A
        double precision, dimension(:,:), allocatable :: B
        double precision, dimension(:,:), allocatable :: C

        contains
            procedure :: run => run_dsyr2k
            procedure :: call_benchmark => call_dsyr2k
    end type DSYR2KBenchmark 

    type, public, extends(Benchmark) :: SSYR2KBenchmark
        integer(int64) :: n
        integer(int64) :: k

        real :: alpha = 1.0
        real :: beta = 0.0

        real, dimension(:,:), allocatable :: A
        real, dimension(:,:), allocatable :: B
        real, dimension(:,:), allocatable :: C

        contains
            procedure :: run => run_ssyr2k
            procedure :: call_benchmark => call_ssyr2k
    end type SSYR2KBenchmark

contains
    subroutine run_dgemm(self, blas_name)
        class(DGEMMBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name

        integer :: i, iunit
        real(real64) :: avg_gflops

        self%name = "DGEMM"

        self%min_exp = 4
        self%max_exp = 14
        self%base = 2

        call self%open_results_file(iunit)
        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = self%min_exp, self%max_exp
            self%m = self%base**i
            self%n = self%base**i
            self%k = self%base**i

            allocate(self%A(self%m , self%n))
            allocate(self%B(self%n , self%k))
            allocate(self%C(self%k , self%n))

            call random_number(self%A)
            call random_number(self%B)
            call random_number(self%C)

            self%num_flops = 2 * self%m * self%n * self%k

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%A)
            deallocate(self%B)
            deallocate(self%C)
        end do

        write(iunit, '(A)') ''

        close(iunit)

    end subroutine run_dgemm

    subroutine run_sgemm(self, blas_name)
        class(SGEMMBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        
        integer :: i, iunit
        real(real64) :: avg_gflops

        self%name = "SGEMM"

        self%min_exp = 4
        self%max_exp = 14
        self%base = 2

        call self%open_results_file(iunit)
        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = self%min_exp, self%max_exp
            self%m = self%base**i
            self%n = self%base**i
            self%k = self%base**i

            allocate(self%A(self%m , self%n))
            allocate(self%B(self%n , self%k))
            allocate(self%C(self%k , self%n))

            call random_number(self%A)
            call random_number(self%B)
            call random_number(self%C)

            self%num_flops = 2 * self%m * self%n * self%k

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%A)
            deallocate(self%B)
            deallocate(self%C)
        end do

        write(iunit, '(A)') ''
        
        close(iunit)

    end subroutine run_sgemm

    subroutine run_dsyrk(self, blas_name)
        class(DSYRKBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        
        integer :: i, iunit
        real(real64) :: avg_gflops

        self%name = "DSYRK"

        self%min_exp = 4
        self%max_exp = 14
        self%base = 2
        
        call self%open_results_file(iunit)
        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = self%min_exp, self%max_exp
            self%n = self%base**i
            self%k = self%base**i

            allocate(self%A(self%n , self%k))
            allocate(self%C(self%n , self%n))

            call random_number(self%A)
            call random_number(self%C)

            self%num_flops = self%n * self%n * self%k

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%A)
            deallocate(self%C)
        end do

        write(iunit, '(A)') ''
        
        close(iunit)

    end subroutine run_dsyrk

    subroutine run_ssyrk(self, blas_name)
        class(SSYRKBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        
        integer :: i, iunit
        real(real64) :: avg_gflops

        self%name = "SSYRK"

        self%min_exp = 4
        self%max_exp = 14
        self%base = 2

        call self%open_results_file(iunit)
        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = self%min_exp, self%max_exp
            self%n = self%base**i
            self%k = self%base**i

            allocate(self%A(self%n , self%k))
            allocate(self%C(self%n , self%n))

            call random_number(self%A)
            call random_number(self%C)

            self%num_flops = self%n * self%n * self%k

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%A)
            deallocate(self%C)
        end do

        write(iunit, '(A)') ''
        
        close(iunit)

    end subroutine run_ssyrk

    subroutine run_dsyr2k(self, blas_name)
        class(DSYR2KBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        
        integer :: i, iunit
        real(real64) :: avg_gflops

        self%name = "DSYR2K"

        self%min_exp = 4
        self%max_exp = 14
        self%base = 2
        
        call self%open_results_file(iunit)
        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = self%min_exp, self%max_exp
            self%n = self%base**i
            self%k = self%base**i

            allocate(self%A(self%n , self%k))
            allocate(self%B(self%n , self%k))
            allocate(self%C(self%n , self%n))

            call random_number(self%A)
            call random_number(self%B)
            call random_number(self%C)

            self%num_flops = self%n * self%n * self%k

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%A)
            deallocate(self%B)
            deallocate(self%C)
        end do

        write(iunit, '(A)') ''
        
        close(iunit)

    end subroutine run_dsyr2k

    subroutine run_ssyr2k(self, blas_name)
        class(SSYR2KBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name
        
        integer :: i, iunit
        real(real64) :: avg_gflops

        self%name = "SSYR2K"

        self%min_exp = 4
        self%max_exp = 14
        self%base = 2

        call self%open_results_file(iunit)
        write(iunit, '(2A)', advance='no') blas_name, ','

        do i = self%min_exp, self%max_exp
            self%n = self%base**i
            self%k = self%base**i

            allocate(self%A(self%n , self%k))
            allocate(self%B(self%n , self%k))
            allocate(self%C(self%n , self%n))

            call random_number(self%A)
            call random_number(self%B)
            call random_number(self%C)

            self%num_flops = self%n * self%n * self%k

            avg_gflops = self%time_benchmark(100)

            write(iunit, '(F11.7,A)', advance='no') avg_gflops, ','

            deallocate(self%A)
            deallocate(self%B)
            deallocate(self%C)
        end do

        write(iunit, '(A)') ''
        
        close(iunit)

    end subroutine run_ssyr2k

    subroutine call_dgemm(self)
        class(DGEMMBenchmark), intent(inout) :: self
        call dgemm(&
            "N",&
            "N",&
            self%m,&
            self%n,&
            self%k,&
            self%alpha,&
            self%A,&
            self%m,&
            self%B,&
            self%k,&
            self%beta,&
            self%C,&
            self%n&
        )
    end subroutine call_dgemm

    subroutine call_sgemm(self)
        class(SGEMMBenchmark), intent(inout) :: self
        call sgemm(&
            "N",&
            "N",&
            self%m,&
            self%n,&
            self%k,&
            self%alpha,&
            self%A,&
            self%m,&
            self%B,&
            self%k,&
            self%beta,&
            self%C,&
            self%n&
        )
    end subroutine call_sgemm

    subroutine call_dsyrk(self)
        class(DSYRKBenchmark), intent(inout) :: self
        call dsyrk(&
            "U",&
            "N",&
            self%n,&
            self%k,&
            self%alpha,&
            self%A,&
            self%n,&
            self%beta,&
            self%C,&
            self%n&
        )
    end subroutine call_dsyrk

    subroutine call_ssyrk(self)
        class(SSYRKBenchmark), intent(inout) :: self
        call ssyrk(&
            "U",&
            "N",&
            self%n,&
            self%k,&
            self%alpha,&
            self%A,&
            self%n,&
            self%beta,&
            self%C,&
            self%n&
        )
    end subroutine call_ssyrk

    subroutine call_dsyr2k(self)
        class(DSYR2KBenchmark), intent(inout) :: self
        call dsyr2k(&
            "U",&
            "N",&
            self%n,&
            self%k,&
            self%alpha,&
            self%A,&
            self%n,&
            self%B,&
            self%n,&
            self%beta,&
            self%C,&
            self%n&
        )
    end subroutine call_dsyr2k

    subroutine call_ssyr2k(self)
        class(SSYR2KBenchmark), intent(inout) :: self
        call ssyr2k(&
            "U",&
            "N",&
            self%n,&
            self%k,&
            self%alpha,&
            self%A,&
            self%n,&
            self%B,&
            self%n,&
            self%beta,&
            self%C,&
            self%n&
        )
    end subroutine call_ssyr2k
end module blas_l3_benchmarks