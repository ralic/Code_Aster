        interface
          subroutine lcmmjs(nomfam,nbsys,tbsys)
            character(len=16) :: nomfam
            integer :: nbsys
            real(kind=8) :: tbsys(30,6)
          end subroutine lcmmjs
        end interface
