        interface
          subroutine virhol(nbvari,vintm,vintp,advihy,vihrho,rho110,&
     &dp1,dp2,dpad,cliq,dt,alpliq,signe,rho11,rho11m,retcom)
            integer :: nbvari
            real(kind=8) :: vintm(nbvari)
            real(kind=8) :: vintp(nbvari)
            integer :: advihy
            integer :: vihrho
            real(kind=8) :: rho110
            real(kind=8) :: dp1
            real(kind=8) :: dp2
            real(kind=8) :: dpad
            real(kind=8) :: cliq
            real(kind=8) :: dt
            real(kind=8) :: alpliq
            real(kind=8) :: signe
            real(kind=8) :: rho11
            real(kind=8) :: rho11m
            integer :: retcom
          end subroutine virhol
        end interface
