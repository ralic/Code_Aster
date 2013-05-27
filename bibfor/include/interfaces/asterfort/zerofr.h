        interface
          subroutine zerofr(intini,algo,func,x1,x2,tol,itmax,solu,iret&
     &,iter)
            integer :: intini
            character(*) :: algo
            real(kind=8) :: func
            external func
            real(kind=8) :: x1
            real(kind=8) :: x2
            real(kind=8) :: tol
            integer :: itmax
            real(kind=8) :: solu
            integer :: iret
            integer :: iter
          end subroutine zerofr
        end interface
