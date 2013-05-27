        interface
          subroutine viemma(nbvari,vintm,vintp,advico,vicphi,phi0,dp1,&
     &dp2,signe,sat,em,phi,phim,retcom)
            integer :: nbvari
            real(kind=8) :: vintm(nbvari)
            real(kind=8) :: vintp(nbvari)
            integer :: advico
            integer :: vicphi
            real(kind=8) :: phi0
            real(kind=8) :: dp1
            real(kind=8) :: dp2
            real(kind=8) :: signe
            real(kind=8) :: sat
            real(kind=8) :: em
            real(kind=8) :: phi
            real(kind=8) :: phim
            integer :: retcom
          end subroutine viemma
        end interface
