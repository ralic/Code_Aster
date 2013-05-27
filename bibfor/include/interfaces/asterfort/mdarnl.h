        interface
          subroutine mdarnl(isto1,ipas,t,dt,nbmode,depgen,vitgen,&
     &accgen,isto2,nbchoc,saucho,nbscho,isto3,nbrede,saured,saredi,isto4&
     &,nbrevi,saurev,sarevi,depsto,vitsto,accsto,passto,iorsto,temsto,&
     &fchost,dchost,vchost,ichost,vint,iredst,dredst,irevst,drevst)
            integer :: nbchoc
            integer :: isto1
            integer :: ipas
            real(kind=8) :: t
            real(kind=8) :: dt
            integer :: nbmode
            real(kind=8) :: depgen(*)
            real(kind=8) :: vitgen(*)
            real(kind=8) :: accgen(*)
            integer :: isto2
            real(kind=8) :: saucho(nbchoc,*)
            integer :: nbscho
            integer :: isto3
            integer :: nbrede
            real(kind=8) :: saured(*)
            integer :: saredi(*)
            integer :: isto4
            integer :: nbrevi
            real(kind=8) :: saurev(*)
            integer :: sarevi(*)
            real(kind=8) :: depsto(*)
            real(kind=8) :: vitsto(*)
            real(kind=8) :: accsto(*)
            real(kind=8) :: passto(*)
            integer :: iorsto(*)
            real(kind=8) :: temsto(*)
            real(kind=8) :: fchost(*)
            real(kind=8) :: dchost(*)
            real(kind=8) :: vchost(*)
            integer :: ichost(*)
            real(kind=8) :: vint(*)
            integer :: iredst(*)
            real(kind=8) :: dredst(*)
            integer :: irevst(*)
            real(kind=8) :: drevst(*)
          end subroutine mdarnl
        end interface
