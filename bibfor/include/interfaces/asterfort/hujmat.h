        interface
          subroutine hujmat(mod,imat,tempf,materf,ndt,ndi,nvi)
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: tempf
            real(kind=8) :: materf(22,2)
            integer :: ndt
            integer :: ndi
            integer :: nvi
          end subroutine hujmat
        end interface
