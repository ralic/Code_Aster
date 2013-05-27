        interface
          subroutine mdchan(motfac,ioc,iliai,mdgene,typnum,repere,xjeu&
     &,nbnli,noecho,parcho)
            integer :: nbnli
            character(len=10) :: motfac
            integer :: ioc
            integer :: iliai
            character(len=24) :: mdgene
            character(len=16) :: typnum
            character(len=8) :: repere
            real(kind=8) :: xjeu
            character(len=8) :: noecho(nbnli,*)
            real(kind=8) :: parcho(nbnli,*)
          end subroutine mdchan
        end interface
