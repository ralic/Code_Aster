        interface
          subroutine chgref(geomi,x,y,bidim)
            character(len=19) :: geomi
            real(kind=8) :: x(3)
            real(kind=8) :: y(3)
            logical :: bidim
          end subroutine chgref
        end interface
