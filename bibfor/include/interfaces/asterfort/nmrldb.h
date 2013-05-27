        interface
          subroutine nmrldb(solveu,lmat,resu,nbsm,cncine)
            character(len=19) :: solveu
            integer :: lmat
            real(kind=8) :: resu(*)
            integer :: nbsm
            character(len=19) :: cncine
          end subroutine nmrldb
        end interface
