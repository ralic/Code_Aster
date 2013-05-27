        interface
          subroutine viporo(nbvari,vintm,vintp,advico,vicphi,phi0,&
     &depsv,alpha0,dt,dp1,dp2,signe,sat,cs,biot,phi,phim,retcom)
            integer :: nbvari
            real(kind=8) :: vintm(nbvari)
            real(kind=8) :: vintp(nbvari)
            integer :: advico
            integer :: vicphi
            real(kind=8) :: phi0
            real(kind=8) :: depsv
            real(kind=8) :: alpha0
            real(kind=8) :: dt
            real(kind=8) :: dp1
            real(kind=8) :: dp2
            real(kind=8) :: signe
            real(kind=8) :: sat
            real(kind=8) :: cs
            real(kind=8) :: biot
            real(kind=8) :: phi
            real(kind=8) :: phim
            integer :: retcom
          end subroutine viporo
        end interface
