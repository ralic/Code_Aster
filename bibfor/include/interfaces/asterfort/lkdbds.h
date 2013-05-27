        interface
          subroutine lkdbds(nmat,mater,i1,devsig,nvi,vint,para,val,&
     &dbetds,dbetdi)
            integer :: nvi
            integer :: nmat
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: i1
            real(kind=8) :: devsig(6)
            real(kind=8) :: vint(nvi)
            real(kind=8) :: para(3)
            integer :: val
            real(kind=8) :: dbetds(6)
            real(kind=8) :: dbetdi
          end subroutine lkdbds
        end interface
