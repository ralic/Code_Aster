        interface
          subroutine capaca(rho0,rho11,rho12,rho21,rho22,sat,phi,csigm&
     &,cp11,cp12,cp21,cp22,k0,alpha0,t,coeps,retcom)
            real(kind=8) :: rho0
            real(kind=8) :: rho11
            real(kind=8) :: rho12
            real(kind=8) :: rho21
            real(kind=8) :: rho22
            real(kind=8) :: sat
            real(kind=8) :: phi
            real(kind=8) :: csigm
            real(kind=8) :: cp11
            real(kind=8) :: cp12
            real(kind=8) :: cp21
            real(kind=8) :: cp22
            real(kind=8) :: k0
            real(kind=8) :: alpha0
            real(kind=8) :: t
            real(kind=8) :: coeps
            integer :: retcom
          end subroutine capaca
        end interface
