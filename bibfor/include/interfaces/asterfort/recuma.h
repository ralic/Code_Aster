        interface
          subroutine recuma(mailla,nbma,nbgr,nomma,nomgr,nbto,numnot)
            integer :: nbto
            integer :: nbgr
            integer :: nbma
            character(len=8) :: mailla
            character(len=8) :: nomma(nbma)
            character(len=24) :: nomgr(nbgr)
            integer :: numnot(nbto)
          end subroutine recuma
        end interface
