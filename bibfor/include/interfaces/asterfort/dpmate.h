        interface
          subroutine dpmate(mod,imat,materf,ndt,ndi,nvi,typedp)
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: materf(5,2)
            integer :: ndt
            integer :: ndi
            integer :: nvi
            integer :: typedp
          end subroutine dpmate
        end interface
