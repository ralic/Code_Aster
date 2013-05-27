        interface
          subroutine mmgeom(iresog,ndim,nne,nnm,ffe,ffm,ddffm,geomae,&
     &geomam,tau1,tau2,norm,mprojn,mprojt,geome,geomm,mprt1n,mprt2n,&
     &gene11,gene21)
            integer :: iresog
            integer :: ndim
            integer :: nne
            integer :: nnm
            real(kind=8) :: ffe(9)
            real(kind=8) :: ffm(9)
            real(kind=8) :: ddffm(3,9)
            real(kind=8) :: geomae(9,3)
            real(kind=8) :: geomam(9,3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: norm(3)
            real(kind=8) :: mprojn(3,3)
            real(kind=8) :: mprojt(3,3)
            real(kind=8) :: geome(3)
            real(kind=8) :: geomm(3)
            real(kind=8) :: mprt1n(3,3)
            real(kind=8) :: mprt2n(3,3)
            real(kind=8) :: gene11(3,3)
            real(kind=8) :: gene21(3,3)
          end subroutine mmgeom
        end interface
