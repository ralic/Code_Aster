        interface
          subroutine cfgcpc(resoco,matass,solveu,neq,nbliai,precon,&
     &tole,premax,epsi)
            character(len=24) :: resoco
            character(len=19) :: matass
            character(len=19) :: solveu
            integer :: neq
            integer :: nbliai
            character(len=16) :: precon
            real(kind=8) :: tole
            integer :: premax
            real(kind=8) :: epsi
          end subroutine cfgcpc
        end interface
