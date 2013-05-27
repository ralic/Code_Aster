        interface
          subroutine asmsup(masse,meca,nbmode,neq,nbsup,nsupp,nomsup,&
     &ndir,reasup,tcosup,nume,lordr)
            integer :: nbsup
            integer :: nbmode
            character(len=8) :: masse
            character(len=8) :: meca
            integer :: neq
            integer :: nsupp(*)
            character(len=8) :: nomsup(nbsup,*)
            integer :: ndir(*)
            real(kind=8) :: reasup(nbsup,nbmode,*)
            integer :: tcosup(nbsup,*)
            character(len=14) :: nume
            integer :: lordr(*)
          end subroutine asmsup
        end interface
