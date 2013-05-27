        interface
          subroutine nmgrtg(ndim,nno,poids,kpg,vff,dfdi,def,pff,option&
     &,axi,r,fm,f,dsidep,sign,sigma,matsym,matuu,vectu)
            integer :: ndim
            integer :: nno
            real(kind=8) :: poids
            integer :: kpg
            real(kind=8) :: vff(*)
            real(kind=8) :: dfdi(*)
            real(kind=8) :: def(*)
            real(kind=8) :: pff(*)
            character(len=16) :: option
            logical :: axi
            real(kind=8) :: r
            real(kind=8) :: fm(3,3)
            real(kind=8) :: f(3,3)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: sign(6)
            real(kind=8) :: sigma(6)
            logical :: matsym
            real(kind=8) :: matuu(*)
            real(kind=8) :: vectu(*)
          end subroutine nmgrtg
        end interface
