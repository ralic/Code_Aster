        interface
          subroutine nmel3d(fami,poum,nno,npg,ipoids,ivf,idfde,geom,&
     &typmod,option,imate,compor,lgpg,crit,depl,angmas,dfdi,pff,def,sig,&
     &vi,matuu,vectu,codret)
            integer :: lgpg
            integer :: npg
            integer :: nno
            character(*) :: fami
            character(*) :: poum
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: geom(3,nno)
            character(len=8) :: typmod(*)
            character(len=16) :: option
            integer :: imate
            character(len=16) :: compor(4)
            real(kind=8) :: crit(3)
            real(kind=8) :: depl(1:3,1:nno)
            real(kind=8) :: angmas(3)
            real(kind=8) :: dfdi(nno,3)
            real(kind=8) :: pff(6,nno,nno)
            real(kind=8) :: def(6,nno,3)
            real(kind=8) :: sig(6,npg)
            real(kind=8) :: vi(lgpg,npg)
            real(kind=8) :: matuu(*)
            real(kind=8) :: vectu(3,nno)
            integer :: codret
          end subroutine nmel3d
        end interface
