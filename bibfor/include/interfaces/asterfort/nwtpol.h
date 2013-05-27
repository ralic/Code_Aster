        interface
          subroutine nwtpol(deg,coef,rac)
            integer :: deg
            real(kind=8) :: coef(deg+1)
            real(kind=8) :: rac
          end subroutine nwtpol
        end interface
