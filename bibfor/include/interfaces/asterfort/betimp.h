        interface
          subroutine betimp(nmat,mater,sig,vind,vinf,elgeom,nseui1,&
     &nseui2,nseui3,nseui4,sige,sigd)
            integer :: nmat
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: vind(*)
            real(kind=8) :: vinf(*)
            real(kind=8) :: elgeom(*)
            integer :: nseui1
            integer :: nseui2
            integer :: nseui3
            integer :: nseui4
            real(kind=8) :: sige(6)
            real(kind=8) :: sigd(6)
          end subroutine betimp
        end interface
