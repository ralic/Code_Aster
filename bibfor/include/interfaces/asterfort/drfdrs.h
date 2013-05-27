        interface
          subroutine drfdrs(q,parame,h0,sigc,rgdev,duds,dfds)
            real(kind=8) :: q(6)
            real(kind=8) :: parame(5)
            real(kind=8) :: h0
            real(kind=8) :: sigc
            real(kind=8) :: rgdev
            real(kind=8) :: duds(6)
            real(kind=8) :: dfds(6)
          end subroutine drfdrs
        end interface
