module benchmark_base
    implicit none

    type, abstract :: Benchmark
        integer :: num_flops
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
            class(Benchmark), intent(inout) :: self
        end subroutine setup_interface
    
        subroutine run_interface(self)
            import
            class(Benchmark), intent(inout) :: self
        end subroutine run_interface
    end interface
end module benchmark_base
