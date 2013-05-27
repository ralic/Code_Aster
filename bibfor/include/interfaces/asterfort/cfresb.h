        interface
          subroutine cfresb(ndim,lag2d,typlia,fctf,tang,rtx,rty,rtz)
            integer :: ndim
            logical :: lag2d
            character(len=2) :: typlia
            real(kind=8) :: fctf(3)
            real(kind=8) :: tang(6)
            real(kind=8) :: rtx
            real(kind=8) :: rty
            real(kind=8) :: rtz
          end subroutine cfresb
        end interface
