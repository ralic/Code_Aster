        interface
          subroutine mdfcho(nbmode,depgen,vitgen,accgen,fexgen,masgen,&
     &phicar,pulsa2,amogen,nbchoc,logcho,dplmod,parcho,noecho,saucho,&
     &temps,nofdep,nofvit,nofacc,nbexci,psidel,nonmot)
            integer :: nbexci
            integer :: nbchoc
            integer :: nbmode
            real(kind=8) :: depgen(*)
            real(kind=8) :: vitgen(*)
            real(kind=8) :: accgen(*)
            real(kind=8) :: fexgen(*)
            real(kind=8) :: masgen(*)
            real(kind=8) :: phicar(*)
            real(kind=8) :: pulsa2(*)
            real(kind=8) :: amogen(*)
            integer :: logcho(nbchoc,*)
            real(kind=8) :: dplmod(nbchoc,nbmode,*)
            real(kind=8) :: parcho(nbchoc,*)
            character(len=8) :: noecho(nbchoc,*)
            real(kind=8) :: saucho(nbchoc,*)
            real(kind=8) :: temps
            character(len=8) :: nofdep(nbexci)
            character(len=8) :: nofvit(nbexci)
            character(len=8) :: nofacc(nbexci)
            real(kind=8) :: psidel(nbchoc,nbexci,*)
            character(len=8) :: nonmot
          end subroutine mdfcho
        end interface
