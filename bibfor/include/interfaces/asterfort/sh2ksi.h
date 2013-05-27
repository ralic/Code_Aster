        interface
          subroutine sh2ksi(npint,xxg,xyg,xzg,bksi)
            integer :: npint
            real(kind=8) :: xxg(20)
            real(kind=8) :: xyg(20)
            real(kind=8) :: xzg(20)
            real(kind=8) :: bksi(3,20,20)
          end subroutine sh2ksi
        end interface
