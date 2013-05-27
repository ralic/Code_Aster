        interface
          subroutine nueqch(erreur,chamno,noma,nbno,numno,nomcmp,nueq)
            character(len=1) :: erreur
            character(len=19) :: chamno
            character(len=8) :: noma
            integer :: nbno
            integer :: numno(*)
            character(len=8) :: nomcmp(*)
            integer :: nueq(*)
          end subroutine nueqch
        end interface
