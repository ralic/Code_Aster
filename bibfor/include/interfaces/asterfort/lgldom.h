        interface
          subroutine lgldom(nbmat,mater,yf,fiter)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: yf(10)
            real(kind=8) :: fiter
          end subroutine lgldom
        end interface
