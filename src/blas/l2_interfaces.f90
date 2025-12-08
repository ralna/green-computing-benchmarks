module l2_interfaces
    implicit none (external)
    private
    public dgemv, sgemv

    interface
        subroutine dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
            use iso_fortran_env, only: int64
            implicit none (external)

            character(len=*), intent(in) :: trans
            integer(int64), intent(in) :: m, n, lda
            integer, intent(in) :: incx, incy
            double precision, intent(in) :: alpha, beta
            double precision, intent(in) :: a(lda, n)
            double precision, intent(in) :: x(n)
            double precision, intent(inout) :: y(m)
        end subroutine dgemv
        
        subroutine sgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
            use iso_fortran_env, only: int64
            implicit none (external)

            character(len=*), intent(in) :: trans
            integer(int64), intent(in) :: m, n, lda
            integer, intent(in) :: incx, incy
            real, intent(in) :: alpha, beta
            real, intent(in) :: a(lda, m)
            real, intent(in) :: x(n)
            real, intent(inout) :: y(m)
        end subroutine sgemv
    end interface

end module l2_interfaces
