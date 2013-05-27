        interface
          subroutine massup(option,ndim,dlns,nno,nnos,mate,phenom,npg,&
     &ipoids,idfde,geom,vff1,imatuu,icodre,igeom,ivf)
            integer :: npg
            integer :: nno
            integer :: ndim
            character(len=16) :: option
            integer :: dlns
            integer :: nnos
            integer :: mate
            character(len=16) :: phenom
            integer :: ipoids
            integer :: idfde
            real(kind=8) :: geom(ndim,nno)
            real(kind=8) :: vff1(nno,npg)
            integer :: imatuu
            integer :: icodre
            integer :: igeom
            integer :: ivf
          end subroutine massup
        end interface
