        interface
          subroutine angcou(coor,zk1,izk,icoude,zk2,rayon,theta,angl1,&
     &angl2,angl3,pgl1,pgl2,pgl3,omega,dn1n2,epsi,crit,zk3)
            real(kind=8) :: coor(9)
            real(kind=8) :: zk1(3)
            integer :: izk
            integer :: icoude
            real(kind=8) :: zk2(3)
            real(kind=8) :: rayon
            real(kind=8) :: theta
            real(kind=8) :: angl1(3)
            real(kind=8) :: angl2(3)
            real(kind=8) :: angl3(3)
            real(kind=8) :: pgl1(3,3)
            real(kind=8) :: pgl2(3,3)
            real(kind=8) :: pgl3(3,3)
            real(kind=8) :: omega
            real(kind=8) :: dn1n2
            real(kind=8) :: epsi
            character(len=8) :: crit
            real(kind=8) :: zk3(3)
          end subroutine angcou
        end interface
