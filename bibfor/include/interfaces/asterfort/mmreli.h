        interface
          subroutine mmreli(alias,nno,ndim,coorma,coorpt,ksi1,ksi2,&
     &dksi1,dksi2,alpha)
            character(len=8) :: alias
            integer :: nno
            integer :: ndim
            real(kind=8) :: coorma(27)
            real(kind=8) :: coorpt(3)
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            real(kind=8) :: dksi1
            real(kind=8) :: dksi2
            real(kind=8) :: alpha
          end subroutine mmreli
        end interface
