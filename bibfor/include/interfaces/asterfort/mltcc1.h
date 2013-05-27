        interface
          subroutine mltcc1(nbloc,ncbloc,decal,supnd,fils,frere,seq,&
     &lgsn,lfront,adress,local,adpile,nbass,pile,lgpile,adper,t1,t2,&
     &factol,factou,typsym,ad,eps,ier,nbb,cl,cu)
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
            complex(kind=8) :: pile(*)
            integer :: lgpile
            integer :: adper(*)
            complex(kind=8) :: t1(*)
            complex(kind=8) :: t2(*)
            character(len=24) :: factol
            character(len=24) :: factou
            integer :: typsym
            integer :: ad(*)
            real(kind=8) :: eps
            integer :: ier
            complex(kind=8) :: cl(nbb,nbb,*)
            complex(kind=8) :: cu(nbb,nbb,*)
          end subroutine mltcc1
        end interface
