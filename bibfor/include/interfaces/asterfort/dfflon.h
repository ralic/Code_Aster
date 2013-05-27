        interface
          subroutine dfflon(geom,nonoff,nomnoe,inoff,nbnoff,typfon,d)
            real(kind=8) :: geom(*)
            character(len=8) :: nonoff(*)
            character(len=24) :: nomnoe
            integer :: inoff
            integer :: nbnoff
            character(len=8) :: typfon
            real(kind=8) :: d
          end subroutine dfflon
        end interface
