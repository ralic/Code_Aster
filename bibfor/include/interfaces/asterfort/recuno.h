        interface
          subroutine recuno(mailla,nbno,nbgr,nomno,nomgr,nbto,numnot)
            integer :: nbto
            integer :: nbgr
            integer :: nbno
            character(len=8) :: mailla
            character(len=8) :: nomno(nbno)
            character(len=24) :: nomgr(nbgr)
            integer :: numnot(nbto)
          end subroutine recuno
        end interface
