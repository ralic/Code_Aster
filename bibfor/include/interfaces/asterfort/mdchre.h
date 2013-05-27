        interface
          subroutine mdchre(motfac,ioc,iliai,mdgene,typnum,repere,&
     &nbnli,parcho,lnoue2)
            integer :: nbnli
            character(len=10) :: motfac
            integer :: ioc
            integer :: iliai
            character(len=24) :: mdgene
            character(len=16) :: typnum
            character(len=8) :: repere
            real(kind=8) :: parcho(nbnli,*)
            logical :: lnoue2
          end subroutine mdchre
        end interface
