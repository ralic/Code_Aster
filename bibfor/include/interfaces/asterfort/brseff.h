        interface
          subroutine brseff(k0,mu0,e0s,e0d,sigeff)
            real(kind=8) :: k0
            real(kind=8) :: mu0
            real(kind=8) :: e0s
            real(kind=8) :: e0d(6)
            real(kind=8) :: sigeff(6)
          end subroutine brseff
        end interface
