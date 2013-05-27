        interface
          subroutine calcdp(crit,seuil,dt,rprim,mutrbe,sigm0,epsi0,&
     &coefm,dp,iret)
            real(kind=8) :: crit(3)
            real(kind=8) :: seuil
            real(kind=8) :: dt
            real(kind=8) :: rprim
            real(kind=8) :: mutrbe
            real(kind=8) :: sigm0
            real(kind=8) :: epsi0
            real(kind=8) :: coefm
            real(kind=8) :: dp
            integer :: iret
          end subroutine calcdp
        end interface
