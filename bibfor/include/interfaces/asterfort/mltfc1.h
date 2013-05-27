        interface
          subroutine mltfc1(nbloc,ncbloc,decal,supnd,fils,frere,seq,&
     &lgsn,lfront,adress,local,adpile,nbass,pile,lgpile,adper,t1,t2,&
     &factol,factou,typsym,ad,eps,ier,nbb,cl,cu,diag)
            integer :: nbb
            integer :: nbloc
            integer :: ncbloc(*)
            integer :: decal(*)
            integer :: supnd(*)
            integer :: fils(*)
            integer :: frere(*)
            integer :: seq(*)
            integer :: lgsn(*)
            integer :: lfront(*)
            integer :: adress(*)
            integer(kind=4) :: local(*)
            integer :: adpile(*)
            integer :: nbass(*)
            real(kind=8) :: pile(*)
            integer :: lgpile
            integer :: adper(*)
            real(kind=8) :: t1(*)
            real(kind=8) :: t2(*)
            character(len=24) :: factol
            character(len=24) :: factou
            integer :: typsym
            integer :: ad(*)
            real(kind=8) :: eps
            integer :: ier
            real(kind=8) :: cl(nbb,nbb,*)
            real(kind=8) :: cu(nbb,nbb,*)
            real(kind=8) :: diag(*)
          end subroutine mltfc1
        end interface
