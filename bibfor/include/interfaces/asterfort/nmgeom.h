        interface
          subroutine nmgeom(ndim,nno,axi,grand,geom,kpg,ipoids,ivf,&
     &idfde,depl,ldfdi,poids,dfdi,f,eps,r)
            integer :: nno
            integer :: ndim
            logical :: axi
            logical :: grand
            real(kind=8) :: geom(ndim,nno)
            integer :: kpg
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: depl(ndim,nno)
            logical :: ldfdi
            real(kind=8) :: poids
            real(kind=8) :: dfdi(nno,ndim)
            real(kind=8) :: f(3,3)
            real(kind=8) :: eps(6)
            real(kind=8) :: r
          end subroutine nmgeom
        end interface
