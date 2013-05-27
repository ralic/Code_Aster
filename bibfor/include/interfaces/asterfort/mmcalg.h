        interface
          subroutine mmcalg(ndim,nnm,ddffm,geomam,tau1,tau2,norm,&
     &mprt1n,mprt2n,gene11,gene21)
            integer :: ndim
            integer :: nnm
            real(kind=8) :: ddffm(3,9)
            real(kind=8) :: geomam(9,3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: norm(3)
            real(kind=8) :: mprt1n(3,3)
            real(kind=8) :: mprt2n(3,3)
            real(kind=8) :: gene11(3,3)
            real(kind=8) :: gene21(3,3)
          end subroutine mmcalg
        end interface
