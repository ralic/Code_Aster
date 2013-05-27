        interface
          subroutine pipepe(pilo,ndim,nno,npg,ipoids,ivf,idfde,geom,&
     &typmod,mate,compor,lgpg,deplm,sigm,vim,ddepl,depl0,depl1,copilo,&
     &dfdi,elgeom,iborne,ictau)
            integer :: lgpg
            integer :: npg
            integer :: ndim
            character(len=16) :: pilo
            integer :: nno
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: geom(ndim,*)
            character(len=8) :: typmod(*)
            integer :: mate
            character(len=16) :: compor(*)
            real(kind=8) :: deplm(*)
            real(kind=8) :: sigm(2*ndim,npg)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: ddepl(*)
            real(kind=8) :: depl0(*)
            real(kind=8) :: depl1(*)
            real(kind=8) :: copilo(5,npg)
            real(kind=8) :: dfdi(*)
            real(kind=8) :: elgeom(10,*)
            integer :: iborne
            integer :: ictau
          end subroutine pipepe
        end interface
