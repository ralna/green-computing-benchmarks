module l1_interfaces
    implicit none (external)
    private
    public dasum, daxpy

    interface
        subroutine dasum(n, x, incx)
            use iso_fortran_env, only: int64
            implicit none (external)

            integer(int64), intent(in) :: n
            double precision, intent(in) :: x(n)
            integer, intent(in) :: incx

        end subroutine dasum

        subroutine daxpy(n, alpha, x, incx, y, incy)
            use iso_fortran_env, only: int64
            integer(int64), intent(in) :: n
            double precision, intent(in) :: alpha
            double precision, intent(in) :: x(n)
            integer, intent(in) :: incx
            double precision, intent(inout) :: y(n)
            integer, intent(in) :: incy

        end subroutine daxpy
    end interface

end module l1_interfaces
