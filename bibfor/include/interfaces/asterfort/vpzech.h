        interface
          subroutine vpzech(d,z,low,high,mm,neq,iz)
            integer :: iz
            real(kind=8) :: d(1)
            real(kind=8) :: z(iz,1)
            integer :: low
            integer :: high
            integer :: mm
            integer :: neq
          end subroutine vpzech
        end interface
