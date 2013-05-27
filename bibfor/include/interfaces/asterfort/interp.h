        interface
          subroutine interp(tabx,taby,necr,x,y,iseg)
            real(kind=8) :: tabx(*)
            real(kind=8) :: taby(*)
            integer :: necr
            real(kind=8) :: x
            real(kind=8) :: y
            integer :: iseg
          end subroutine interp
        end interface
