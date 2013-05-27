        interface
          subroutine lkd2sh(nmat,materf,varh,dhds,devsig,rcos3t,d2shds&
     &,iret)
            integer :: nmat
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: varh(3)
            real(kind=8) :: dhds(6)
            real(kind=8) :: devsig(6)
            real(kind=8) :: rcos3t
            real(kind=8) :: d2shds(6,6)
            integer :: iret
          end subroutine lkd2sh
        end interface
