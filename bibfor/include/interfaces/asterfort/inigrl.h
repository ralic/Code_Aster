        interface
          subroutine inigrl(ligrel,igrel,nmax,adtabl,k24tab,nval)
            integer :: nmax
            character(*) :: ligrel
            integer :: igrel
            integer :: adtabl(nmax)
            character(len=24) :: k24tab(nmax)
            integer :: nval
          end subroutine inigrl
        end interface
