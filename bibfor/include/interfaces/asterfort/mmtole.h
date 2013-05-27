        interface
          subroutine mmtole(alias,nno,ndim,coorma,toleou,ksi1,ksi2,&
     &tau1,tau2,iproj)
            character(len=8) :: alias
            integer :: nno
            integer :: ndim
            real(kind=8) :: coorma(27)
            real(kind=8) :: toleou
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            integer :: iproj
          end subroutine mmtole
        end interface
