        interface
          subroutine calcdy(mu,k,f0,devg,devgii,traceg,dfdl,delta,dy)
            real(kind=8) :: mu
            real(kind=8) :: k
            real(kind=8) :: f0
            real(kind=8) :: devg(6)
            real(kind=8) :: devgii
            real(kind=8) :: traceg
            real(kind=8) :: dfdl
            real(kind=8) :: delta
            real(kind=8) :: dy(10)
          end subroutine calcdy
        end interface
