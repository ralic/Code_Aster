        interface
          subroutine dfda2d(kpg,nno,poids,sdfrde,sdfrdk,sdedx,sdedy,&
     &sdkdx,sdkdy,sdfdx,sdfdy,geom,jac)
            integer :: kpg
            integer :: nno
            real(kind=8) :: poids
            real(kind=8) :: sdfrde(4,4)
            real(kind=8) :: sdfrdk(4,4)
            real(kind=8) :: sdedx(4)
            real(kind=8) :: sdedy(4)
            real(kind=8) :: sdkdx(4)
            real(kind=8) :: sdkdy(4)
            real(kind=8) :: sdfdx(4,4)
            real(kind=8) :: sdfdy(4,4)
            real(kind=8) :: geom(2,4)
            real(kind=8) :: jac
          end subroutine dfda2d
        end interface
