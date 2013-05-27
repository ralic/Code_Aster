        interface
          subroutine porea2(nno,nc,geom,gamma,pgl,xl)
            integer :: nno
            integer :: nc
            real(kind=8) :: geom(3,nno)
            real(kind=8) :: gamma
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: xl
          end subroutine porea2
        end interface
