        interface
          subroutine mmtang(ndim,nno,coorma,dff,tau1,tau2)
            integer :: ndim
            integer :: nno
            real(kind=8) :: coorma(27)
            real(kind=8) :: dff(2,9)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
          end subroutine mmtang
        end interface
