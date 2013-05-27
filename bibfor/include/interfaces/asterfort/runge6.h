        interface
          subroutine runge6(ipif,deltat,tpgp,tpgm,hpgm,hpgp,err)
            integer :: ipif
            real(kind=8) :: deltat
            real(kind=8) :: tpgp
            real(kind=8) :: tpgm
            real(kind=8) :: hpgm
            real(kind=8) :: hpgp
            real(kind=8) :: err
          end subroutine runge6
        end interface
