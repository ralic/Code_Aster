        interface
          subroutine dilele(option,typmod,npi,ndim,dimuel,nddls,nddlm,&
     &nno,nnos,nnom,axi,regula,dimcon,ipoids,ipoid2,ivf,ivf2,interp,&
     &idfde,idfde2,compor,geom,deplp,contp,imate,dimdef,matuu,vectu)
            integer :: dimdef
            integer :: dimcon
            integer :: dimuel
            integer :: ndim
            integer :: npi
            character(len=16) :: option
            character(len=8) :: typmod(2)
            integer :: nddls
            integer :: nddlm
            integer :: nno
            integer :: nnos
            integer :: nnom
            logical :: axi
            integer :: regula(6)
            integer :: ipoids
            integer :: ipoid2
            integer :: ivf
            integer :: ivf2
            character(len=2) :: interp
            integer :: idfde
            integer :: idfde2
            character(len=16) :: compor(*)
            real(kind=8) :: geom(ndim,*)
            real(kind=8) :: deplp(dimuel)
            real(kind=8) :: contp(dimcon*npi)
            integer :: imate
            real(kind=8) :: matuu(dimuel*dimuel)
            real(kind=8) :: vectu(dimuel)
          end subroutine dilele
        end interface
