        interface
          subroutine cfgcrl(resoco,neq,nbliai,matass,solveu,alpha)
            character(len=24) :: resoco
            integer :: neq
            integer :: nbliai
            character(len=19) :: matass
            character(len=19) :: solveu
            real(kind=8) :: alpha
          end subroutine cfgcrl
        end interface
