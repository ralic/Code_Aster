        interface
          subroutine ecrbas(nbsauv,nbnl,nbmode,depgen,vitgen,accgen,&
     &temps,jordre,ptemp,depbut,vitbut,forbut,redepg,revitg,reaccg,&
     &retemp,reordr,reptem,redepb,revitb,reforb)
            integer :: nbmode
            integer :: nbnl
            integer :: nbsauv
            real(kind=8) :: depgen(nbmode,*)
            real(kind=8) :: vitgen(nbmode,*)
            real(kind=8) :: accgen(nbmode,*)
            real(kind=8) :: temps(*)
            integer :: jordre(*)
            real(kind=8) :: ptemp(*)
            real(kind=8) :: depbut(nbnl,3,*)
            real(kind=8) :: vitbut(nbnl,3,*)
            real(kind=8) :: forbut(nbnl,3,*)
            real(kind=8) :: redepg(*)
            real(kind=8) :: revitg(*)
            real(kind=8) :: reaccg(*)
            real(kind=8) :: retemp(*)
            integer :: reordr(*)
            real(kind=8) :: reptem(*)
            real(kind=8) :: redepb(*)
            real(kind=8) :: revitb(*)
            real(kind=8) :: reforb(*)
          end subroutine ecrbas
        end interface
