! utility files
module slate_utils
    use iso_c_binding
    public

contains
    subroutine random_Tile_r64( T )
        use, intrinsic :: iso_fortran_env
        use slate
        implicit none

        type(slate_Tile_r64)            :: T
        integer(kind=c_int64_t)         :: m, n, lda
        type(c_ptr)                     :: T_data_c
        real(kind=c_double), pointer    :: A(:)
        integer                         :: i, j

        m   = slate_Tile_mb_r64( T )
        n   = slate_Tile_nb_r64( T )
        lda = slate_Tile_stride_r64( T )
        T_data_c = slate_Tile_data_r64( T )
        call c_f_pointer( T_data_c, A, [lda*n] )

        do j = 0, n-1
            do i = 1, m
                A( i + j*lda ) = rand()
            end do
        end do
    end subroutine

        subroutine random_Tile_r32( T )
        use, intrinsic :: iso_fortran_env
        use slate
        implicit none

        type(slate_Tile_r32)            :: T
        integer(kind=c_int64_t)         :: m, n, lda
        type(c_ptr)                     :: T_data_c
        real(kind=c_float), pointer     :: A(:)
        integer                         :: i, j

        m   = slate_Tile_mb_r32( T )
        n   = slate_Tile_nb_r32( T )
        lda = slate_Tile_stride_r32( T )
        T_data_c = slate_Tile_data_r32( T )
        call c_f_pointer( T_data_c, A, [lda*n] )

        do j = 0, n-1
            do i = 1, m
                A( i + j*lda ) = rand()
            end do
        end do
    end subroutine

    subroutine random_Matrix_r64( A )
        use, intrinsic :: iso_fortran_env
        use slate
        implicit none

        type(c_ptr), value              :: A
        type(slate_Tile_r64)            :: T
        integer(kind=c_int64_t)         :: i, j

        do j = 0, slate_Matrix_nt_r64( A )-1
            do i = 0, slate_Matrix_mt_r64( A )-1
                if (slate_Matrix_tileIsLocal_r64( A, i, j )) then
                    T = slate_Matrix_at_r64( A, i, j )
                    call random_Tile_r64( T )
                end if
            end do
        end do
    end subroutine

    subroutine random_Matrix_r32( A )
        use, intrinsic :: iso_fortran_env
        use slate
        implicit none

        type(c_ptr), value              :: A
        type(slate_Tile_r32)            :: T
        integer(kind=c_int64_t)         :: i, j

        do j = 0, slate_Matrix_nt_r32( A )-1
            do i = 0, slate_Matrix_mt_r32( A )-1
                if (slate_Matrix_tileIsLocal_r32( A, i, j )) then
                    T = slate_Matrix_at_r32( A, i, j )
                    call random_Tile_r32( T )
                end if
            end do
        end do
    end subroutine

    subroutine grid_size( mpi_size, p, q )
        use, intrinsic :: iso_fortran_env
        implicit none

        integer(kind=c_int) :: mpi_size, p, q
        real                :: mpi_size_real

        mpi_size_real = mpi_size

        do p = floor( sqrt( mpi_size_real ) ), 1, -1
            q = mpi_size / p

            if (p*q == mpi_size) then
               return
            end if
        end do
    end subroutine
end module slate_utils
