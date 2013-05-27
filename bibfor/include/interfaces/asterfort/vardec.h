        interface
          subroutine vardec(xloc,xloc0,ivar,dt0,tole)
            real(kind=8) :: xloc(*)
            real(kind=8) :: xloc0(*)
            integer :: ivar
            real(kind=8) :: dt0
            real(kind=8) :: tole
          end subroutine vardec
        end interface
