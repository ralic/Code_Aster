        interface
          subroutine cast3d(proj,gamma,dh,def,nno,kpg,nub,nu,dsidep,&
     &calbn,bn,jac,matuu)
            integer :: proj
            real(kind=8) :: gamma(4,8)
            real(kind=8) :: dh(4,24)
            real(kind=8) :: def(6,3,8)
            integer :: nno
            integer :: kpg
            real(kind=8) :: nub
            real(kind=8) :: nu
            real(kind=8) :: dsidep(6,6)
            logical :: calbn
            real(kind=8) :: bn(6,3,8)
            real(kind=8) :: jac
            real(kind=8) :: matuu(*)
          end subroutine cast3d
        end interface
