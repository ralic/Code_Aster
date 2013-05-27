        interface
          subroutine vpzvph(nn,acc,prerel,hh,ih,valpr,valpi,icnt,ier)
            integer :: ih
            integer :: nn
            real(kind=8) :: acc
            real(kind=8) :: prerel
            real(kind=8) :: hh(ih,nn)
            real(kind=8) :: valpr(nn)
            real(kind=8) :: valpi(nn)
            integer :: icnt(nn)
            integer :: ier
          end subroutine vpzvph
        end interface
