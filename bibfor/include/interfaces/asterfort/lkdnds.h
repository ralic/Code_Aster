        interface
          subroutine lkdnds(nmat,materf,i1,devsig,bprimp,nvi,vint,val,&
     &para,dndsig)
            integer :: nvi
            integer :: nmat
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: i1
            real(kind=8) :: devsig(6)
            real(kind=8) :: bprimp
            real(kind=8) :: vint(nvi)
            integer :: val
            real(kind=8) :: para(3)
            real(kind=8) :: dndsig(6,6)
          end subroutine lkdnds
        end interface
