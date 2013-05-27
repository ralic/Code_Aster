        interface
          subroutine lkfsxi(nmat,materf,i1,devsig,dshds,plas,xi,para,&
     &vara,dfdsdx,dpardx)
            integer :: nmat
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: i1
            real(kind=8) :: devsig(6)
            real(kind=8) :: dshds(6)
            logical :: plas
            real(kind=8) :: xi
            real(kind=8) :: para(3)
            real(kind=8) :: vara(4)
            real(kind=8) :: dfdsdx(6)
            real(kind=8) :: dpardx(3)
          end subroutine lkfsxi
        end interface
