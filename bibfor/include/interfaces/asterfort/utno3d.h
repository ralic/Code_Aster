        interface
          subroutine utno3d(ifm,niv,nsomm,ifa,tymvol,igeom,xn,yn,zn,&
     &jac,idfdx,idfdy,hf,poids3,npgf,noe)
            integer :: ifm
            integer :: niv
            integer :: nsomm
            integer :: ifa
            integer :: tymvol
            integer :: igeom
            real(kind=8) :: xn(9)
            real(kind=8) :: yn(9)
            real(kind=8) :: zn(9)
            real(kind=8) :: jac(9)
            integer :: idfdx
            integer :: idfdy
            real(kind=8) :: hf
            real(kind=8) :: poids3(9)
            integer :: npgf
            integer :: noe(9,6,4)
          end subroutine utno3d
        end interface
