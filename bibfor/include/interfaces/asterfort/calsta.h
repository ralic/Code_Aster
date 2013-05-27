        interface
          subroutine calsta(proj,gamma,dh,def,nno,kpg,sig,tmp,kk,kkd,&
     &matuu,dsidep,jac)
            integer :: proj
            real(kind=8) :: gamma(4)
            real(kind=8) :: dh(8)
            real(kind=8) :: def(4,4,2)
            integer :: nno
            integer :: kpg
            real(kind=8) :: sig(6)
            real(kind=8) :: tmp
            integer :: kk
            integer :: kkd
            real(kind=8) :: matuu(*)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: jac
          end subroutine calsta
        end interface
