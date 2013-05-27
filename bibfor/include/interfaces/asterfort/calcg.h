        interface
          subroutine calcg(dfds,vecn,g,devg,traceg,devgii)
            real(kind=8) :: dfds(6)
            real(kind=8) :: vecn(6)
            real(kind=8) :: g(6)
            real(kind=8) :: devg(6)
            real(kind=8) :: traceg
            real(kind=8) :: devgii
          end subroutine calcg
        end interface
