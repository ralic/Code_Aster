        interface
          subroutine statim(nbobst,nbpt,temps,fcho,vgli,defpla,wk1,wk2&
     &,wk3,tdebut,tfin,nbloc,offset,trepos,nbclas,noecho,intitu,nomres)
            integer :: nbobst
            integer :: nbpt
            real(kind=8) :: temps(*)
            real(kind=8) :: fcho(*)
            real(kind=8) :: vgli(*)
            real(kind=8) :: defpla(*)
            real(kind=8) :: wk1(*)
            real(kind=8) :: wk2(*)
            real(kind=8) :: wk3(*)
            real(kind=8) :: tdebut
            real(kind=8) :: tfin
            integer :: nbloc
            real(kind=8) :: offset
            real(kind=8) :: trepos
            integer :: nbclas
            character(len=8) :: noecho(*)
            character(len=8) :: intitu(*)
            character(*) :: nomres
          end subroutine statim
        end interface
