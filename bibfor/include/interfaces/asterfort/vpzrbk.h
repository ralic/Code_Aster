        interface
          subroutine vpzrbk(z,h,d,mm,izh,k,l)
            integer :: izh
            real(kind=8) :: z(izh,1)
            real(kind=8) :: h(izh,1)
            real(kind=8) :: d(1)
            integer :: mm
            integer :: k
            integer :: l
          end subroutine vpzrbk
        end interface
