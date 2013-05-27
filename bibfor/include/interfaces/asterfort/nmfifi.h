        interface
          subroutine nmfifi(npg,typmod,geom,sigma,fint)
            integer :: npg
            character(len=8) :: typmod(2)
            real(kind=8) :: geom(2,4)
            real(kind=8) :: sigma(2,npg)
            real(kind=8) :: fint(8)
          end subroutine nmfifi
        end interface
