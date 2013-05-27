        interface
          subroutine lkd2fs(nmat,materf,para,vara,varh,i1,devsig,&
     &ds2hds,d2shds,d2fds2,iret)
            integer :: nmat
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: para(3)
            real(kind=8) :: vara(4)
            real(kind=8) :: varh(3)
            real(kind=8) :: i1
            real(kind=8) :: devsig(6)
            real(kind=8) :: ds2hds(6)
            real(kind=8) :: d2shds(6,6)
            real(kind=8) :: d2fds2(6,6)
            integer :: iret
          end subroutine lkd2fs
        end interface
