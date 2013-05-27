        interface
          subroutine depsh6(loop,bloc,ueloc,deps,d)
            integer :: loop
            real(kind=8) :: bloc(6,18)
            real(kind=8) :: ueloc(3,6)
            real(kind=8) :: deps(6)
            real(kind=8) :: d(9)
          end subroutine depsh6
        end interface
