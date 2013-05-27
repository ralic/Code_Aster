        interface
          subroutine ecrgen(iordre,nbmode,tc,dt,depg,vitg,accg,depgen,&
     &vitgen,accgen,temps,jordre,ptemps)
            integer :: nbmode
            integer :: iordre
            real(kind=8) :: tc
            real(kind=8) :: dt
            real(kind=8) :: depg(*)
            real(kind=8) :: vitg(*)
            real(kind=8) :: accg(*)
            real(kind=8) :: depgen(nbmode,*)
            real(kind=8) :: vitgen(nbmode,*)
            real(kind=8) :: accgen(nbmode,*)
            real(kind=8) :: temps(*)
            integer :: jordre(*)
            real(kind=8) :: ptemps(*)
          end subroutine ecrgen
        end interface
