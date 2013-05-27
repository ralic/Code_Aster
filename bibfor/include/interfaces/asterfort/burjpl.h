        interface
          subroutine burjpl(nmat,mater,nr,drdy,dsde)
            integer :: nr
            integer :: nmat
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: drdy(nr,nr)
            real(kind=8) :: dsde(6,6)
          end subroutine burjpl
        end interface
