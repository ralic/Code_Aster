        interface
          subroutine lkdgds(nmat,materf,para,vara,devsig,i1,val,ds2hds&
     &,vecn,dfds,bprimp,nvi,vint,dhds,dgds,iret)
            integer :: nvi
            integer :: nmat
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: para(3)
            real(kind=8) :: vara(4)
            real(kind=8) :: devsig(6)
            real(kind=8) :: i1
            integer :: val
            real(kind=8) :: ds2hds(6)
            real(kind=8) :: vecn(6)
            real(kind=8) :: dfds(6)
            real(kind=8) :: bprimp
            real(kind=8) :: vint(nvi)
            real(kind=8) :: dhds(6)
            real(kind=8) :: dgds(6,6)
            integer :: iret
          end subroutine lkdgds
        end interface
