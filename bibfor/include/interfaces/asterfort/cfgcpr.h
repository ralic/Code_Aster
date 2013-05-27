        interface
          subroutine cfgcpr(resoco,matass,solveu,neq,nbliai,search,&
     &alpha)
            character(len=24) :: resoco
            character(len=19) :: matass
            character(len=19) :: solveu
            integer :: neq
            integer :: nbliai
            character(len=16) :: search
            real(kind=8) :: alpha
          end subroutine cfgcpr
        end interface
