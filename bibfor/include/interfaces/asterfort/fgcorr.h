        interface
          subroutine fgcorr(nbcycl,sigmin,sigmax,method,su,rcorr)
            integer :: nbcycl
            real(kind=8) :: sigmin(*)
            real(kind=8) :: sigmax(*)
            character(*) :: method
            real(kind=8) :: su
            real(kind=8) :: rcorr(*)
          end subroutine fgcorr
        end interface
