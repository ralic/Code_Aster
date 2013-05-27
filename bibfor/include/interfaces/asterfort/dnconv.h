        interface
          subroutine dnconv(n,ritzr,ritzi,bounds,tol,nconv)
            integer :: n
            real(kind=8) :: ritzr(n)
            real(kind=8) :: ritzi(n)
            real(kind=8) :: bounds(n)
            real(kind=8) :: tol
            integer :: nconv
          end subroutine dnconv
        end interface
