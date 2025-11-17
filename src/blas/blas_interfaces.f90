module blas_interfaces
    implicit none (external)
    private
    public dgemm, sgemm, dgemv, dasum, sasum, sgemv, daxpy, dsyrk, ssyrk, dsyr2k, ssyr2k

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

        subroutine sgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
            use iso_fortran_env, only: int64
            implicit none (external)

            character(len=*), intent(in) :: transa, transb
            integer(int64), intent(in) :: m, n, k, lda, ldb, ldc
            real, intent(in) :: alpha, beta
            real, intent(in) :: a(lda, k)
            real, intent(in) :: b(ldb, n)
            real, intent(inout) :: c(ldc, n)
        end subroutine sgemm

        subroutine dsyrk(uplo, trans, n, k, alpha, a, lda, beta, c, ldc)
            use iso_fortran_env, only: int64
            implicit none (external)

            character(len=*), intent(in) :: uplo, trans
            integer(int64), intent(in) :: n, k, lda, ldc
            double precision, intent(in) :: alpha, beta
            double precision, intent(in) :: a(lda, k)
            double precision, intent(inout) :: c(ldc, n)
        end subroutine dsyrk

        subroutine ssyrk(uplo, trans, n, k, alpha, a, lda, beta, c, ldc)
            use iso_fortran_env, only: int64
            implicit none (external)

            character(len=*), intent(in) :: uplo, trans
            integer(int64), intent(in) :: n, k, lda, ldc
            real, intent(in) :: alpha, beta
            real, intent(in) :: a(lda, k)
            real, intent(inout) :: c(ldc, n)
        end subroutine ssyrk

        subroutine dsyr2k(uplo, trans, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
            use iso_fortran_env, only: int64
            implicit none (external)

            character(len=*), intent(in) :: uplo, trans
            integer(int64), intent(in) :: n, k, lda, ldb, ldc
            double precision, intent(in) :: alpha, beta
            double precision, intent(in) :: a(lda, k), b(lda, k)
            double precision, intent(inout) :: c(ldc, n)
        end subroutine dsyr2k

        subroutine ssyr2k(uplo, trans, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
            use iso_fortran_env, only: int64
            implicit none (external)

            character(len=*), intent(in) :: uplo, trans
            integer(int64), intent(in) :: n, k, lda, ldb, ldc
            real, intent(in) :: alpha, beta
            real, intent(in) :: a(lda, k), b(lda, k)
            real, intent(inout) :: c(ldc, n)
        end subroutine ssyr2k

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

        subroutine dasum(n, x, incx)
            use iso_fortran_env, only: int64
            implicit none (external)

            integer(int64), intent(in) :: n
            double precision, intent(in) :: x(n)
            integer, intent(in) :: incx

        end subroutine dasum

        subroutine sasum(n, x, incx)
            use iso_fortran_env, only: int64
            implicit none (external)

            integer(int64), intent(in) :: n
            real, intent(in) :: x(n)
            integer, intent(in) :: incx

        end subroutine sasum

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

end module blas_interfaces
