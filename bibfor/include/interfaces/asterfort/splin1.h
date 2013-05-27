        interface
          subroutine splin1(x,y,d2y,n,ptx,dyptx,iret)
            real(kind=8) :: x(*)
            real(kind=8) :: y(*)
            real(kind=8) :: d2y(*)
            integer :: n
            real(kind=8) :: ptx
            real(kind=8) :: dyptx
            integer :: iret
          end subroutine splin1
        end interface
