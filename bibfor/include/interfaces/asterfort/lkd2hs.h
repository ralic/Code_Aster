        interface
          subroutine lkd2hs(nmat,materf,devsig,sii,rcos3t,dhds,d2hds2)
            integer :: nmat
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: devsig(6)
            real(kind=8) :: sii
            real(kind=8) :: rcos3t
            real(kind=8) :: dhds(6)
            real(kind=8) :: d2hds2(6,6)
          end subroutine lkd2hs
        end interface
