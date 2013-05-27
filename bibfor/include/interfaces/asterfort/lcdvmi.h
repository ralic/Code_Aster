        interface
          subroutine lcdvmi(sigma,y,f,dfds,d2fds,seq)
            real(kind=8) :: sigma(6)
            real(kind=8) :: y
            real(kind=8) :: f
            real(kind=8) :: dfds(6)
            real(kind=8) :: d2fds(6,6)
            real(kind=8) :: seq
          end subroutine lcdvmi
        end interface
