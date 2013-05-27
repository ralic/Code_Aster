        interface
          subroutine pmfpti(num,xl,xi,wi,b,g)
            integer :: num
            real(kind=8) :: xl
            real(kind=8) :: xi
            real(kind=8) :: wi
            real(kind=8) :: b(4)
            real(kind=8) :: g
          end subroutine pmfpti
        end interface
