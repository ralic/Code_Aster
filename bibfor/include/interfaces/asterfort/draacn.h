        interface
          subroutine draacn(deg,poly,nbroot,root)
            integer :: deg
            real(kind=8) :: poly(deg+1)
            integer :: nbroot
            real(kind=8) :: root(deg)
          end subroutine draacn
        end interface
