        interface
          subroutine statch(nbobst,nbpt,temps,dloc,fcho,vgli,iadh,wk1,&
     &wk2,wk3,iwk4,tdebut,tfin,nbloc,offset,trepos,noecho,intitu,nomres)
            integer :: nbobst
            integer :: nbpt
            real(kind=8) :: temps(*)
            real(kind=8) :: dloc(*)
            real(kind=8) :: fcho(*)
            real(kind=8) :: vgli(*)
            integer :: iadh(*)
            real(kind=8) :: wk1(*)
            real(kind=8) :: wk2(*)
            real(kind=8) :: wk3(*)
            integer :: iwk4(*)
            real(kind=8) :: tdebut
            real(kind=8) :: tfin
            integer :: nbloc
            real(kind=8) :: offset
            real(kind=8) :: trepos
            character(len=8) :: noecho(*)
            character(len=8) :: intitu(*)
            character(*) :: nomres
          end subroutine statch
        end interface
