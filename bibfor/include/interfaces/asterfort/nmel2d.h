        interface
          subroutine nmel2d(fami,poum,nno,npg,ipoids,ivf,idfde,geom,&
     &typmod,option,imate,compor,lgpg,crit,idepl,angmas,dfdi,pff,def,sig&
     &,vi,matuu,ivectu,codret)
            integer :: lgpg
            integer :: npg
            integer :: nno
            character(*) :: fami
            character(*) :: poum
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: geom(2,nno)
            character(len=8) :: typmod(*)
            character(len=16) :: option
            integer :: imate
            character(len=16) :: compor(4)
            real(kind=8) :: crit(3)
            integer :: idepl
            real(kind=8) :: angmas(3)
            real(kind=8) :: dfdi(nno,2)
            real(kind=8) :: pff(4,nno,nno)
            real(kind=8) :: def(4,nno,2)
            real(kind=8) :: sig(4,npg)
            real(kind=8) :: vi(lgpg,npg)
            real(kind=8) :: matuu(*)
            integer :: ivectu
            integer :: codret
          end subroutine nmel2d
        end interface
