        interface
          subroutine lklini(sigf,nr,yd,dy)
            integer :: nr
            real(kind=8) :: sigf(6)
            real(kind=8) :: yd(nr)
            real(kind=8) :: dy(nr)
          end subroutine lklini
        end interface
