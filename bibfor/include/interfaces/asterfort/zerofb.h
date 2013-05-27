        interface
          subroutine zerofb(func,x1,x2,tol,itmax,zbrent,iret,iter)
            real(kind=8) :: func
            external func
            real(kind=8) :: x1
            real(kind=8) :: x2
            real(kind=8) :: tol
            integer :: itmax
            real(kind=8) :: zbrent
            integer :: iret
            integer :: iter
          end subroutine zerofb
        end interface
