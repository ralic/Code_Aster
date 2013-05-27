        interface
          subroutine lkhtet(nbmat,mater,rcos3t,h0e,h0c,htheta)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: rcos3t
            real(kind=8) :: h0e
            real(kind=8) :: h0c
            real(kind=8) :: htheta
          end subroutine lkhtet
        end interface
