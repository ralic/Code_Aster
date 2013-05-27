        interface
          subroutine burdfi(bfi,cfi,nr,yd,dy)
            integer :: nr
            real(kind=8) :: bfi(6,6)
            real(kind=8) :: cfi(6,6)
            real(kind=8) :: yd(nr)
            real(kind=8) :: dy(nr)
          end subroutine burdfi
        end interface
