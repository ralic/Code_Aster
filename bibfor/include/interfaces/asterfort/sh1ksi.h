        interface
          subroutine sh1ksi(npint,xxg,xyg,xzg,bksi)
            integer :: npint
            real(kind=8) :: xxg(15)
            real(kind=8) :: xyg(15)
            real(kind=8) :: xzg(15)
            real(kind=8) :: bksi(3,15,15)
          end subroutine sh1ksi
        end interface
