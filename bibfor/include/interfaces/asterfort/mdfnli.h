        interface
          subroutine mdfnli(nbmode,depgen,vitgen,accgen,fexgen,masgen,&
     &phicar,pulsa2,amogen,nbchoc,logcho,dplmod,parcho,noecho,saucho,&
     &nbrede,dplred,parred,fonred,saured,saredi,nbrevi,dplrev,fonrev,&
     &temps,nofdep,nofvit,nofacc,nbexci,psidel,monmot,nbrfis,fk,dfk,&
     &angini,foncp,numpas,nbpal,dt,dtsto,vrotat,typal,finpal,cnpal,&
     &prdeff,conv,fsauv)
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
            integer :: logcho(*)
            real(kind=8) :: dplmod(nbchoc,nbmode,*)
            real(kind=8) :: parcho(*)
            character(len=8) :: noecho(*)
            real(kind=8) :: saucho(*)
            integer :: nbrede
            real(kind=8) :: dplred(*)
            real(kind=8) :: parred(*)
            character(len=8) :: fonred(*)
            real(kind=8) :: saured(*)
            integer :: saredi(*)
            integer :: nbrevi
            real(kind=8) :: dplrev(*)
            character(len=8) :: fonrev(*)
            real(kind=8) :: temps
            character(len=8) :: nofdep(nbexci)
            character(len=8) :: nofvit(nbexci)
            character(len=8) :: nofacc(nbexci)
            real(kind=8) :: psidel(nbchoc,nbexci,*)
            character(len=8) :: monmot
            integer :: nbrfis
            character(len=8) :: fk(2)
            character(len=8) :: dfk(2)
            real(kind=8) :: angini
            character(len=8) :: foncp
            integer :: numpas
            integer :: nbpal
            real(kind=8) :: dt
            real(kind=8) :: dtsto
            real(kind=8) :: vrotat
            character(len=6) :: typal(20)
            character(len=3) :: finpal(20)
            character(len=8) :: cnpal(20)
            logical :: prdeff
            real(kind=8) :: conv
            real(kind=8) :: fsauv(20,3)
          end subroutine mdfnli
        end interface
