        interface
          subroutine vpztr1(mm,nn,ia,a,x,y,alfa)
            integer :: ia
            integer :: mm
            integer :: nn
            real(kind=8) :: a(ia,*)
            real(kind=8) :: x(*)
            real(kind=8) :: y(*)
            real(kind=8) :: alfa
          end subroutine vpztr1
        end interface
