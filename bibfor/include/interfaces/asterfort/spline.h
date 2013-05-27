        interface
          subroutine spline(x,y,n,dy1,dyn,d2y,iret)
            real(kind=8) :: x(*)
            real(kind=8) :: y(*)
            integer :: n
            real(kind=8) :: dy1
            real(kind=8) :: dyn
            real(kind=8) :: d2y(*)
            integer :: iret
          end subroutine spline
        end interface
