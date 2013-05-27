        interface
          subroutine epsreg(npi,ipoids,ipoid2,ivf,ivf2,idfde,idfde2,&
     &geom,dimdef,dimuel,ndim,nddls,nddlm,nno,nnos,nnom,axi,regula,deplp&
     &,defgep)
            integer :: ndim
            integer :: dimuel
            integer :: dimdef
            integer :: npi
            integer :: ipoids
            integer :: ipoid2
            integer :: ivf
            integer :: ivf2
            integer :: idfde
            integer :: idfde2
            real(kind=8) :: geom(ndim,*)
            integer :: nddls
            integer :: nddlm
            integer :: nno
            integer :: nnos
            integer :: nnom
            logical :: axi
            integer :: regula(6)
            real(kind=8) :: deplp(dimuel)
            real(kind=8) :: defgep(npi*dimdef)
          end subroutine epsreg
        end interface
