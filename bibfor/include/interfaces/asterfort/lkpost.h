        interface
          subroutine lkpost(imate,tempd,sigf,nvi,vip)
            integer :: nvi
            integer :: imate
            real(kind=8) :: tempd
            real(kind=8) :: sigf(6)
            real(kind=8) :: vip(nvi)
          end subroutine lkpost
        end interface
