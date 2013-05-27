        interface
          subroutine mmjeux(alias,nno,ndim,coorma,ksi1,ksi2,coorpt,&
     &jeupm,dist)
            character(len=8) :: alias
            integer :: nno
            integer :: ndim
            real(kind=8) :: coorma(27)
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            real(kind=8) :: coorpt(3)
            real(kind=8) :: jeupm
            real(kind=8) :: dist(3)
          end subroutine mmjeux
        end interface
