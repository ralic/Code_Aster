        interface
          subroutine calcfp(mutrbe,rprim,seuil,dt,dp,sigm0,epsi0,coefm&
     &,fplas,fprim,dfprim)
            real(kind=8) :: mutrbe
            real(kind=8) :: rprim
            real(kind=8) :: seuil
            real(kind=8) :: dt
            real(kind=8) :: dp
            real(kind=8) :: sigm0
            real(kind=8) :: epsi0
            real(kind=8) :: coefm
            real(kind=8) :: fplas
            real(kind=8) :: fprim
            real(kind=8) :: dfprim
          end subroutine calcfp
        end interface
