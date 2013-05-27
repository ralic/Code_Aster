        interface
          subroutine calcds(hook,devg,devgii,dfds,dfdg,dsde)
            real(kind=8) :: hook(6,6)
            real(kind=8) :: devg(6)
            real(kind=8) :: devgii
            real(kind=8) :: dfds(6)
            real(kind=8) :: dfdg
            real(kind=8) :: dsde(6,6)
          end subroutine calcds
        end interface
