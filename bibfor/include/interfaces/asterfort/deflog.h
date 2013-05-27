        interface
          subroutine deflog(ndim,f,epsl,gn,lamb,logl,iret)
            integer :: ndim
            real(kind=8) :: f(3,3)
            real(kind=8) :: epsl(6)
            real(kind=8) :: gn(3,3)
            real(kind=8) :: lamb(3)
            real(kind=8) :: logl(3)
            integer :: iret
          end subroutine deflog
        end interface
