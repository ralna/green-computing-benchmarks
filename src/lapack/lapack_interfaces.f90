module lapack_interfaces
    implicit none (external)
    private
    public dgesv

    interface
        subroutine dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
            use iso_fortran_env, only: int64
            implicit none (external)

            integer(int64), intent(in) :: n, nrhs, lda, ldb
            double precision, intent(inout) :: a(lda, n)
            double precision, intent(out) :: ipiv(n)
            double precision, intent(inout) :: b(ldb, nrhs)
            integer, intent(out) :: info
        end subroutine dgesv

    end interface

end module lapack_interfaces
