        interface
          subroutine lkdphi(nbmat,mater,de,seuilv,dfdsv,dphi)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: de(6,6)
            real(kind=8) :: seuilv
            real(kind=8) :: dfdsv(6)
            real(kind=8) :: dphi(6)
          end subroutine lkdphi
        end interface
