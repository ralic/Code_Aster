        interface
          subroutine irrlnf(nmat,materf,yf,eloupl,vinf)
            integer :: nmat
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: yf(*)
            real(kind=8) :: eloupl
            real(kind=8) :: vinf(*)
          end subroutine irrlnf
        end interface
