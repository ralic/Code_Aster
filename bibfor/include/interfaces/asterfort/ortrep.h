        interface
          subroutine ortrep(mater,ndim,coor,repere)
            integer :: mater
            integer :: ndim
            real(kind=8) :: coor(3)
            real(kind=8) :: repere(7)
          end subroutine ortrep
        end interface
