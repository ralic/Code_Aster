        interface
          subroutine utin3d(igeom,nsomm,ino,ityp,inst,insold,k8cart,&
     &ltheta,niv,ifm,option,valfp,valfm,noe)
            integer :: igeom
            integer :: nsomm
            integer :: ino
            integer :: ityp
            real(kind=8) :: inst
            real(kind=8) :: insold
            character(len=8) :: k8cart
            logical :: ltheta
            integer :: niv
            integer :: ifm
            integer :: option
            real(kind=8) :: valfp(9)
            real(kind=8) :: valfm(9)
            integer :: noe(9,6,3)
          end subroutine utin3d
        end interface
