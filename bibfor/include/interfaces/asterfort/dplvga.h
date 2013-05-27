        interface
          subroutine dplvga(yate,rho11,rho12,r,t,kh,congem,dimcon,&
     &adcp11,adcp12,ndim,padp,dp11p1,dp11p2,dp12p1,dp12p2,dp21p1,dp21p2,&
     &dp11t,dp12t,dp21t)
            integer :: dimcon
            integer :: yate
            real(kind=8) :: rho11
            real(kind=8) :: rho12
            real(kind=8) :: r
            real(kind=8) :: t
            real(kind=8) :: kh
            real(kind=8) :: congem(dimcon)
            integer :: adcp11
            integer :: adcp12
            integer :: ndim
            real(kind=8) :: padp
            real(kind=8) :: dp11p1
            real(kind=8) :: dp11p2
            real(kind=8) :: dp12p1
            real(kind=8) :: dp12p2
            real(kind=8) :: dp21p1
            real(kind=8) :: dp21p2
            real(kind=8) :: dp11t
            real(kind=8) :: dp12t
            real(kind=8) :: dp21t
          end subroutine dplvga
        end interface
