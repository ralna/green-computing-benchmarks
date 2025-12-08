module l3_interfaces
    implicit none (external)
    private
    public dgemm, dsyrk, dsyr2k

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

        subroutine dsyrk(uplo, trans, n, k, alpha, a, lda, beta, c, ldc)
            use iso_fortran_env, only: int64
            implicit none (external)

            character(len=*), intent(in) :: uplo, trans
            integer(int64), intent(in) :: n, k, lda, ldc
            double precision, intent(in) :: alpha, beta
            double precision, intent(in) :: a(lda, k)
            double precision, intent(inout) :: c(ldc, n)
        end subroutine dsyrk

        subroutine dsyr2k(uplo, trans, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
            use iso_fortran_env, only: int64
            implicit none (external)

            character(len=*), intent(in) :: uplo, trans
            integer(int64), intent(in) :: n, k, lda, ldb, ldc
            double precision, intent(in) :: alpha, beta
            double precision, intent(in) :: a(lda, k), b(lda, k)
            double precision, intent(inout) :: c(ldc, n)
        end subroutine dsyr2k
    end interface

end module l3_interfaces
