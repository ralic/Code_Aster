        interface
          subroutine lcegeo(nno,npg,ipoids,ivf,idfde,geom,typmod,&
     &compor,ndim,dfdi,deplm,ddepl,elgeom)
            integer :: npg
            integer :: nno
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: geom(3,nno)
            character(len=8) :: typmod(2)
            character(len=16) :: compor(*)
            integer :: ndim
            real(kind=8) :: dfdi(nno,3)
            real(kind=8) :: deplm(3,nno)
            real(kind=8) :: ddepl(3,nno)
            real(kind=8) :: elgeom(10,npg)
          end subroutine lcegeo
        end interface
