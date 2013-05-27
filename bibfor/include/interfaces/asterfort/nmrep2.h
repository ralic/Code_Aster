        interface
          subroutine nmrep2(n,r,g,gu,rmin,rmax,rexm,rexp,posopt)
            integer :: n
            real(kind=8) :: r(*)
            real(kind=8) :: g(*)
            real(kind=8) :: gu
            real(kind=8) :: rmin
            real(kind=8) :: rmax
            real(kind=8) :: rexm
            real(kind=8) :: rexp
            integer :: posopt
          end subroutine nmrep2
        end interface
