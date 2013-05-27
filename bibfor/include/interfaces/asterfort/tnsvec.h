        interface
          subroutine tnsvec(choix,ndim,mat,vec,r)
            integer :: ndim
            integer :: choix
            real(kind=8) :: mat(3,3)
            real(kind=8) :: vec(2*ndim)
            real(kind=8) :: r
          end subroutine tnsvec
        end interface
