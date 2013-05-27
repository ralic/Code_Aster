        interface
          subroutine mmresi(alias,nno,ndim,coorma,coorpt,ksi1,ksi2,&
     &valeur)
            character(len=8) :: alias
            integer :: nno
            integer :: ndim
            real(kind=8) :: coorma(27)
            real(kind=8) :: coorpt(3)
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            real(kind=8) :: valeur
          end subroutine mmresi
        end interface
