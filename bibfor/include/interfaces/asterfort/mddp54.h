        interface
          subroutine mddp54(neqgen,depl,vite,acce,fext,dt,dtsto,lflu,&
     &nbexci,idescf,nomfon,coefm,liad,inumor,nbchoc,logcho,dplmod,parcho&
     &,noecho,saucho,nbrede,dplred,parred,fonred,saured,saredi,nbrevi,&
     &dplrev,fonrev,nofdep,nofvit,nofacc,psidel,monmot,nbrfis,fk,dfk,&
     &angini,foncp,nbpal,vrotat,typal,finpal,cnpal,prdeff,conv,fsauv,&
     &typbas,pulsa2,masgen,descmm,riggen,descmr,lamor,descma,work1,temps&
     &,tol,depli,vitei,erde,ervi,kde,kvi,fonca,foncv,istep,rigy,amgy,&
     &nbconv,nbmxcv,vitvar,gyogen,rgygen,amogen,errt)
            integer :: nbchoc
            integer :: neqgen
            real(kind=8) :: depl(*)
            real(kind=8) :: vite(*)
            real(kind=8) :: acce(*)
            real(kind=8) :: fext(*)
            real(kind=8) :: dt
            real(kind=8) :: dtsto
            logical :: lflu
            integer :: nbexci
            integer :: idescf(*)
            character(len=8) :: nomfon(*)
            real(kind=8) :: coefm(*)
            integer :: liad(*)
            integer :: inumor(*)
            integer :: logcho(nbchoc,*)
            real(kind=8) :: dplmod(nbchoc,neqgen,*)
            real(kind=8) :: parcho(*)
            character(len=8) :: noecho(nbchoc,*)
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
            character(len=8) :: nofdep(*)
            character(len=8) :: nofvit(*)
            character(len=8) :: nofacc(*)
            real(kind=8) :: psidel(*)
            character(len=8) :: monmot
            integer :: nbrfis
            character(len=8) :: fk(2)
            character(len=8) :: dfk(2)
            real(kind=8) :: angini
            character(len=8) :: foncp
            integer :: nbpal
            real(kind=8) :: vrotat
            character(len=6) :: typal(20)
            character(len=3) :: finpal(20)
            character(len=8) :: cnpal(20)
            logical :: prdeff
            real(kind=8) :: conv
            real(kind=8) :: fsauv(20,3)
            character(len=16) :: typbas
            real(kind=8) :: pulsa2(*)
            real(kind=8) :: masgen(*)
            integer :: descmm
            real(kind=8) :: riggen(*)
            integer :: descmr
            logical :: lamor
            integer :: descma
            real(kind=8) :: work1(*)
            real(kind=8) :: temps
            real(kind=8) :: tol
            real(kind=8) :: depli(*)
            real(kind=8) :: vitei(*)
            real(kind=8) :: erde(*)
            real(kind=8) :: ervi(*)
            real(kind=8) :: kde(*)
            real(kind=8) :: kvi(*)
            character(len=8) :: fonca
            character(len=8) :: foncv
            integer :: istep
            real(kind=8) :: rigy(*)
            real(kind=8) :: amgy(*)
            integer :: nbconv
            integer :: nbmxcv
            character(len=8) :: vitvar
            real(kind=8) :: gyogen(*)
            real(kind=8) :: rgygen(*)
            real(kind=8) :: amogen(*)
            real(kind=8) :: errt
          end subroutine mddp54
        end interface
