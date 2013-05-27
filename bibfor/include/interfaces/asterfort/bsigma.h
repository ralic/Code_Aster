        interface
          subroutine bsigma(ip,xl,phiy,phiz,b,intpol)
            integer :: ip
            real(kind=8) :: xl
            real(kind=8) :: phiy
            real(kind=8) :: phiz
            real(kind=8) :: b(4,14)
            integer :: intpol
          end subroutine bsigma
        end interface
