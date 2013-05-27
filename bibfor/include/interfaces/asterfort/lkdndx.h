        interface
          subroutine lkdndx(nmat,mater,i1,devsig,bprime,val,para,xi,&
     &dpardx,dndxi)
            integer :: nmat
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: i1
            real(kind=8) :: devsig(6)
            real(kind=8) :: bprime
            integer :: val
            real(kind=8) :: para(3)
            real(kind=8) :: xi
            real(kind=8) :: dpardx(3)
            real(kind=8) :: dndxi(6)
          end subroutine lkdndx
        end interface
