        interface
          subroutine estivd(nbm,dt,vitg,depg,accg0,vitg0,depg0,tetaes,&
     &maxvit,inewto)
            integer :: nbm
            real(kind=8) :: dt
            real(kind=8) :: vitg(*)
            real(kind=8) :: depg(*)
            real(kind=8) :: accg0(*)
            real(kind=8) :: vitg0(*)
            real(kind=8) :: depg0(*)
            real(kind=8) :: tetaes
            real(kind=8) :: maxvit
            integer :: inewto
          end subroutine estivd
        end interface
