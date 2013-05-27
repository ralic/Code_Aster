        interface
          subroutine lcverr(dy,ddy,nr,typ,err)
            real(kind=8) :: dy(*)
            real(kind=8) :: ddy(*)
            integer :: nr
            integer :: typ
            real(kind=8) :: err(*)
          end subroutine lcverr
        end interface
