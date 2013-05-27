        interface
          subroutine nmas2d(fami,nno,npg,ipoids,ivf,idfde,geom,typmod,&
     &option,imate,compor,lgpg,crit,instam,instap,deplm,deplp,angmas,&
     &sigm,vim,dfdi,def,sigp,vip,matuu,vectu,codret)
            integer :: lgpg
            integer :: npg
            integer :: nno
            character(*) :: fami
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: geom(2,nno)
            character(len=8) :: typmod(*)
            character(len=16) :: option
            integer :: imate
            character(len=16) :: compor(4)
            real(kind=8) :: crit(3)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: deplm(1:2,1:nno)
            real(kind=8) :: deplp(1:2,1:nno)
            real(kind=8) :: angmas(3)
            real(kind=8) :: sigm(10,npg)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: dfdi(nno,2)
            real(kind=8) :: def(4,nno,2)
            real(kind=8) :: sigp(10,npg)
            real(kind=8) :: vip(lgpg,npg)
            real(kind=8) :: matuu(*)
            real(kind=8) :: vectu(2,nno)
            integer :: codret
          end subroutine nmas2d
        end interface
