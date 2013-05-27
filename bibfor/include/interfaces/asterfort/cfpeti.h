        interface
          subroutine cfpeti(resoco,neq,nbliai,nbliac,llf,llf1,llf2,rho&
     &,llliai,llliac)
            character(len=24) :: resoco
            integer :: neq
            integer :: nbliai
            integer :: nbliac
            integer :: llf
            integer :: llf1
            integer :: llf2
            real(kind=8) :: rho
            integer :: llliai
            integer :: llliac
          end subroutine cfpeti
        end interface
