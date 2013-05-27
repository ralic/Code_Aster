        interface
          subroutine irmmno(idfimd,nomamd,ndim,nbnoeu,coordo,nomnoe)
            integer :: idfimd
            character(*) :: nomamd
            integer :: ndim
            integer :: nbnoeu
            real(kind=8) :: coordo(*)
            character(*) :: nomnoe(*)
          end subroutine irmmno
        end interface
