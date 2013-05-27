        interface
          subroutine tstpar(itest,nbm,amor,amor0,puls,puls0,dt,dt0)
            integer :: itest
            integer :: nbm
            real(kind=8) :: amor(*)
            real(kind=8) :: amor0(*)
            real(kind=8) :: puls(*)
            real(kind=8) :: puls0(*)
            real(kind=8) :: dt
            real(kind=8) :: dt0
          end subroutine tstpar
        end interface
