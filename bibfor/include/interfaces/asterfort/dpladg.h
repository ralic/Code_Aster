        interface
          subroutine dpladg(yate,rho11,rho12,r,t,kh,congem,dimcon,&
     &adcp11,ndim,padp,dp11p1,dp11p2,dp21p1,dp21p2,dp11t,dp21t)
            integer :: dimcon
            integer :: yate
            real(kind=8) :: rho11
            real(kind=8) :: rho12
            real(kind=8) :: r
            real(kind=8) :: t
            real(kind=8) :: kh
            real(kind=8) :: congem(dimcon)
            integer :: adcp11
            integer :: ndim
            real(kind=8) :: padp
            real(kind=8) :: dp11p1
            real(kind=8) :: dp11p2
            real(kind=8) :: dp21p1
            real(kind=8) :: dp21p2
            real(kind=8) :: dp11t
            real(kind=8) :: dp21t
          end subroutine dpladg
        end interface
