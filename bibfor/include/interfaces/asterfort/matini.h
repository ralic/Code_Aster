        interface
          subroutine matini(nlig,ncol,s,mat)
            integer :: ncol
            integer :: nlig
            real(kind=8) :: s
            real(kind=8) :: mat(nlig,ncol)
          end subroutine matini
        end interface
