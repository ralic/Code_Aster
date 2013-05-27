        interface
          subroutine pipdef(ndim,nno,kpg,ipoids,ivf,idfde,geom,typmod,&
     &compor,deplm,ddepl,depl0,depl1,dfdi,fm,epsm,epsp,epsd)
            integer :: ndim
            integer :: nno
            integer :: kpg
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: geom(ndim,*)
            character(len=8) :: typmod(*)
            character(len=16) :: compor(*)
            real(kind=8) :: deplm(*)
            real(kind=8) :: ddepl(*)
            real(kind=8) :: depl0(*)
            real(kind=8) :: depl1(*)
            real(kind=8) :: dfdi(*)
            real(kind=8) :: fm(3,3)
            real(kind=8) :: epsm(6)
            real(kind=8) :: epsp(6)
            real(kind=8) :: epsd(6)
          end subroutine pipdef
        end interface
