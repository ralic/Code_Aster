        interface
          subroutine dsde2d(ndim,f,dsde,d)
            integer :: ndim
            real(kind=8) :: f(3,3)
            real(kind=8) :: dsde(6,6)
            real(kind=8) :: d(6,6)
          end subroutine dsde2d
        end interface
