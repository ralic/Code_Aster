        interface
          subroutine pacou4(a,n,c,d,sing)
            integer :: n
            real(kind=8) :: a(n,*)
            real(kind=8) :: c(*)
            real(kind=8) :: d(*)
            logical :: sing
          end subroutine pacou4
        end interface
