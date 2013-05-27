        interface
          subroutine cjsmat(mod,imat,tempf,materf,ndt,ndi,nvi,nivcjs)
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: tempf
            real(kind=8) :: materf(14,2)
            integer :: ndt
            integer :: ndi
            integer :: nvi
            character(len=4) :: nivcjs
          end subroutine cjsmat
        end interface
