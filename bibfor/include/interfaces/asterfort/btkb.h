        interface
          subroutine btkb(ndimc,ndimx,nddle,wmatc,btild,wmatcb,ktildi)
            integer :: ndimx
            integer :: ndimc
            integer :: nddle
            real(kind=8) :: wmatc(ndimc,ndimc)
            real(kind=8) :: btild(ndimc,ndimx)
            real(kind=8) :: wmatcb(ndimc,ndimx)
            real(kind=8) :: ktildi(ndimx,ndimx)
          end subroutine btkb
        end interface
