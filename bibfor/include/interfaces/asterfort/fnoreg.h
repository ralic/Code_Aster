        interface
          subroutine fnoreg(dimuel,dimdef,nno,nnos,nnom,ndim,npi,&
     &dimcon,geom,ipoids,ipoid2,ivf,ivf2,idfde,idfde2,nddls,nddlm,axi,&
     &regula,deplm,contm,imate,vectu)
            integer :: dimcon
            integer :: npi
            integer :: ndim
            integer :: dimdef
            integer :: dimuel
            integer :: nno
            integer :: nnos
            integer :: nnom
            real(kind=8) :: geom(ndim,*)
            integer :: ipoids
            integer :: ipoid2
            integer :: ivf
            integer :: ivf2
            integer :: idfde
            integer :: idfde2
            integer :: nddls
            integer :: nddlm
            logical :: axi
            integer :: regula(6)
            real(kind=8) :: deplm(dimuel)
            real(kind=8) :: contm(dimcon*npi)
            integer :: imate
            real(kind=8) :: vectu(dimuel)
          end subroutine fnoreg
        end interface
