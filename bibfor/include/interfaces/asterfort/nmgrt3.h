        interface
          subroutine nmgrt3(nno,poids,kpg,vff,def,pff,option,axi,r,&
     &resi,rigi,dsidep,sign,sigma,matsym,matuu,vectu)
            integer :: nno
            real(kind=8) :: poids
            integer :: kpg
            real(kind=8) :: vff(*)
            real(kind=8) :: def(6,nno,3)
            real(kind=8) :: pff(6,nno,nno)
            character(len=16) :: option
            logical :: axi
            real(kind=8) :: r
            logical :: resi
            logical :: rigi
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: sign(6)
            real(kind=8) :: sigma(6)
            logical :: matsym
            real(kind=8) :: matuu(*)
            real(kind=8) :: vectu(3,nno)
          end subroutine nmgrt3
        end interface
