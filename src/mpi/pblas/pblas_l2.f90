module pblas_l2_benchmarks
    use benchmark_base, only: Benchmark
    ! use blas_interfaces, only: pdgemv
    use iso_fortran_env, only: int64, real64
    implicit none (external)
    private

    external blacs_pinfo, blacs_get, blacs_barrier
    external blacs_gridinfo, blacs_gridinit, blacs_gridexit, blacs_exit
    external descinit
    external pdgemv

    type, public, extends(Benchmark) :: PDGEMVBenchmark
        integer(int64) :: m
        integer(int64) :: n

        double precision :: alpha = 1.0
        double precision :: beta = 1.0

        double precision, dimension(:,:), allocatable :: la
        double precision, dimension(:), allocatable :: lx
        double precision, dimension(:), allocatable :: ly

        integer desca(9), descx(9), descy(9)
        integer :: ictxt

        contains
            procedure :: run => run_pdgemv
            procedure :: call_benchmark => call_pdgemv
    end type PDGEMVBenchmark

contains
    subroutine run_pdgemv(self, blas_name)
        class(PDGEMVBenchmark), intent(inout) :: self
        character(len=16), intent(in) :: blas_name

        integer:: iam,nprocs,nprow,npcol
        integer :: myrow, mycol
        integer:: rsrc,csrc
        integer(int64):: mb,nb
        integer:: llda,lldb,info

        integer :: i, iunit
        real(real64) :: avg_gflops

        call blacs_pinfo(iam,nprocs)
        call blacs_get(-1,0,self%ictxt)

        nprow=2
        npcol=2

        call blacs_gridinit(self%ictxt,'r',nprow,npcol)
        call blacs_gridinfo(self%ictxt,nprow,npcol,myrow,mycol)

        rsrc=0
        csrc=0

        self%min_exp = 4
        self%max_exp = 14
        self%base = 2

        if ((myrow == 0) .and. (mycol == 0)) then
            call self%open_results_file(iunit)
            write(iunit, '(2A)', advance='no') blas_name, ','
        end if

        do i = self%min_exp, self%max_exp
            self%m = self%base**i
            self%n = self%base**i

            mb = self%m / nprow
            nb = self%n / npcol
    
            llda = mb
            lldb = nb
    
            call descinit(self%desca, self%m, self%n, mb, nb, rsrc, csrc, self%ictxt, llda, info)
            call descinit(self%descx, self%m, 1_int64, mb, 1_int64, rsrc, csrc, self%ictxt, lldb, info)
            call descinit(self%descy, self%m, 1_int64, mb, 1_int64, rsrc, csrc, self%ictxt, lldb, info)

            allocate(self%la(mb , nb))
            allocate(self%lx(nb))
            allocate(self%ly(nb))

            call random_number(self%la)
            call random_number(self%lx)
            call random_number(self%ly)
            
            self%num_flops = 2 * self%m * self%n
    
            avg_gflops = self%time_benchmark(100)

            if (myrow == 0 .and. mycol == 0) then
                write(iunit, '(F13.7,A)', advance='no') avg_gflops, ','
            end if

            deallocate(self%la)
            deallocate(self%lx)
            deallocate(self%ly)
        end do
            
        if (myrow == 0 .and. mycol == 0) then
            write(iunit, '(A)') ''
            close(iunit)
        end if

        call blacs_gridexit(self%ictxt)
        call blacs_exit(0)

    end subroutine run_pdgemv

    subroutine call_pdgemv(self)
        class(PDGEMVBenchmark), intent(inout) :: self
        call pdgemv(&
            "N",&
            self%m,&
            self%n,&
            self%alpha,&
            self%la,&
            1,&
            1,&
            self%desca,&
            self%lx,&
            1,&
            1,&
            self%descx,&
            1,&
            self%beta,&
            self%ly,&
            1,&
            1,&
            self%descy,&
            1&
        )
        call blacs_barrier(self%ictxt, 'A')
        return

    end subroutine call_pdgemv
end module pblas_l2_benchmarks