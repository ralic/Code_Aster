        interface
          subroutine mdadap(dti,dtmax,neqgen,pulsat,pulsa2,masgen,&
     &descm,riggen,descr,lamor,amogen,desca,typbas,basemo,tinit,tfin,&
     &dtarch,nbsauv,itemax,prec,xlambd,lflu,nbchoc,logcho,dplmod,parcho,&
     &noecho,nbrede,dplred,parred,fonred,nbrevi,dplrev,fonrev,depsto,&
     &vitsto,accsto,passto,iorsto,temsto,fchost,dchost,vchost,ichost,&
     &iredst,dredst,coefm,liad,inumor,idescf,nofdep,nofvit,nofacc,nomfon&
     &,psidel,monmot,nbpal,dtsto,vrotat,prdeff,method,nomres,nbexci,&
     &irevst,drevst)
            integer :: nbchoc
            integer :: neqgen
            real(kind=8) :: dti
            real(kind=8) :: dtmax
            real(kind=8) :: pulsat(*)
            real(kind=8) :: pulsa2(*)
            real(kind=8) :: masgen(*)
            integer :: descm
            real(kind=8) :: riggen(*)
            integer :: descr
            logical :: lamor
            real(kind=8) :: amogen(*)
            integer :: desca
            character(len=16) :: typbas
            character(len=8) :: basemo
            real(kind=8) :: tinit
            real(kind=8) :: tfin
            real(kind=8) :: dtarch
            integer :: nbsauv
            integer :: itemax
            real(kind=8) :: prec
            real(kind=8) :: xlambd
            logical :: lflu
            integer :: logcho(nbchoc,*)
            real(kind=8) :: dplmod(nbchoc,neqgen,*)
            real(kind=8) :: parcho(*)
            character(len=8) :: noecho(nbchoc,*)
            integer :: nbrede
            real(kind=8) :: dplred(*)
            real(kind=8) :: parred(*)
            character(len=8) :: fonred(*)
            integer :: nbrevi
            real(kind=8) :: dplrev(*)
            character(len=8) :: fonrev(*)
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
            integer :: iredst(*)
            real(kind=8) :: dredst(*)
            real(kind=8) :: coefm(*)
            integer :: liad(*)
            integer :: inumor(*)
            integer :: idescf(*)
            character(len=8) :: nofdep(*)
            character(len=8) :: nofvit(*)
            character(len=8) :: nofacc(*)
            character(len=8) :: nomfon(*)
            real(kind=8) :: psidel(*)
            character(len=8) :: monmot
            integer :: nbpal
            real(kind=8) :: dtsto
            real(kind=8) :: vrotat
            logical :: prdeff
            character(len=16) :: method
            character(len=8) :: nomres
            integer :: nbexci
            integer :: irevst(*)
            real(kind=8) :: drevst(*)
          end subroutine mdadap
        end interface
