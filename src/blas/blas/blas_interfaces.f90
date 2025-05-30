module blas_interfaces
    implicit none (external)
    private
    public dgemm, dgemv, dasum, daxpy

    interface
        subroutine dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
            use iso_fortran_env, only: int64
            implicit none (external)

            character(len=*), intent(in) :: transa, transb
            integer(int64), intent(in) :: m, n, k, lda, ldb, ldc
            double precision, intent(in) :: alpha, beta
            double precision, intent(in) :: a(lda, k)
            double precision, intent(in) :: b(ldb, n)
            double precision, intent(inout) :: c(ldc, n)
        end subroutine dgemm

        subroutine dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
            use iso_fortran_env, only: int64
            implicit none (external)

            character(len=*), intent(in) :: trans
            integer(int64), intent(in) :: m, n, lda
            integer, intent(in) :: incx, incy
            double precision, intent(in) :: alpha, beta
            double precision, intent(in) :: a(lda, m)
            double precision, intent(in) :: x(n)
            double precision, intent(inout) :: y(m)
        end subroutine dgemv

        subroutine dasum(n, x, incx)
            use iso_fortran_env, only: int64
            implicit none (external)

            integer(int64), intent(in) :: n
            double precision, intent(in) :: x(n)
            integer, intent(in) :: incx

        end subroutine dasum

        subroutine daxpy(n, alpha, x, incx, y, incy)
            use iso_fortran_env, only: int64
            implicit none (external)

            integer(int64), intent(in) :: n
            double precision, intent(in) :: alpha
            double precision, intent(in) :: x(n)
            integer, intent(in) :: incx
            double precision, intent(inout) :: y(n)
            integer, intent(in) :: incy

        end subroutine daxpy
    end interface

end module blas_interfaces
