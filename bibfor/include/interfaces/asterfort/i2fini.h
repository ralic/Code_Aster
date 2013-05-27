        interface
          subroutine i2fini(epsi,binf,bsup,tsor,tsex,tm2,adrgt,fini)
            real(kind=8) :: epsi
            real(kind=8) :: binf
            real(kind=8) :: bsup
            real(kind=8) :: tsor(*)
            real(kind=8) :: tsex(*)
            integer :: tm2(*)
            integer :: adrgt
            logical :: fini
          end subroutine i2fini
        end interface
