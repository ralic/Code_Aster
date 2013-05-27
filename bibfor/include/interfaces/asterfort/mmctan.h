        interface
          subroutine mmctan(nommai,alias,nno,ndim,coorma,coorno,itemax&
     &,epsmax,tau1,tau2)
            character(len=8) :: nommai
            character(len=8) :: alias
            integer :: nno
            integer :: ndim
            real(kind=8) :: coorma(27)
            real(kind=8) :: coorno(3)
            integer :: itemax
            real(kind=8) :: epsmax
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
          end subroutine mmctan
        end interface
