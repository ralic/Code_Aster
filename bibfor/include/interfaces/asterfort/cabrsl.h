        interface
          subroutine cabrsl(kpi,ipoids,ipoid2,ivf,ivf2,idfde,idfde2,&
     &geom,dimdef,dimuel,ndim,nddls,nddlm,nno,nnos,nnom,axi,regula,b,&
     &poids,poids2)
            integer :: nnos
            integer :: nno
            integer :: ndim
            integer :: dimuel
            integer :: dimdef
            integer :: kpi
            integer :: ipoids
            integer :: ipoid2
            integer :: ivf
            integer :: ivf2
            integer :: idfde
            integer :: idfde2
            real(kind=8) :: geom(ndim,*)
            integer :: nddls
            integer :: nddlm
            integer :: nnom
            logical :: axi
            integer :: regula(6)
            real(kind=8) :: b(dimdef,dimuel)
            real(kind=8) :: poids
            real(kind=8) :: poids2
          end subroutine cabrsl
        end interface
