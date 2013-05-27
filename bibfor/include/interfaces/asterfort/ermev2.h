        interface
          subroutine ermev2(nno,igeom,ff,sig,nbcmp,dfdx,dfdy,poids,&
     &poiaxi,dsx,dsy,norme)
            integer :: nno
            integer :: igeom
            real(kind=8) :: ff(nno)
            real(kind=8) :: sig(*)
            integer :: nbcmp
            real(kind=8) :: dfdx(9)
            real(kind=8) :: dfdy(9)
            real(kind=8) :: poids
            integer :: poiaxi
            real(kind=8) :: dsx
            real(kind=8) :: dsy
            real(kind=8) :: norme
          end subroutine ermev2
        end interface
