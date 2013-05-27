        interface
          subroutine caldis(fremax,fremin,pas,frexci,nbptmd,nbmode,&
     &lismod,fremod,amomod,nindex,npdsc3,frefin)
            real(kind=8) :: fremax
            real(kind=8) :: fremin
            real(kind=8) :: pas
            character(len=4) :: frexci
            integer :: nbptmd
            integer :: nbmode
            integer :: lismod(*)
            real(kind=8) :: fremod(*)
            real(kind=8) :: amomod(*)
            integer :: nindex
            integer :: npdsc3
            real(kind=8) :: frefin
          end subroutine caldis
        end interface
