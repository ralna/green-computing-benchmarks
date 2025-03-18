module benchmark_base
    use iso_fortran_env, only: int64
    implicit none (external)
    private

    type, public, abstract :: Benchmark
        integer(int64) :: num_flops
        character(len=16) :: name
    contains
        procedure(setup_interface), deferred :: setup
        procedure(run_interface), deferred :: run
    end type Benchmark

    type, public :: BenchmarkContainer
        class(Benchmark), allocatable :: b
    end type BenchmarkContainer

    abstract interface
        subroutine setup_interface(self)
            import
            implicit none (external)
            class(Benchmark), intent(inout) :: self
        end subroutine setup_interface

        subroutine run_interface(self)
            import
            implicit none (external)
            class(Benchmark), intent(inout) :: self
        end subroutine run_interface
    end interface
end module benchmark_base
