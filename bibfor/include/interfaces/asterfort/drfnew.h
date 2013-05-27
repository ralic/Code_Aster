        interface
          subroutine drfnew(devg,devgii,traceg,dfds,dfdg,mu,k,dfdl)
            real(kind=8) :: devg(6)
            real(kind=8) :: devgii
            real(kind=8) :: traceg
            real(kind=8) :: dfds(6)
            real(kind=8) :: dfdg
            real(kind=8) :: mu
            real(kind=8) :: k
            real(kind=8) :: dfdl
          end subroutine drfnew
        end interface
