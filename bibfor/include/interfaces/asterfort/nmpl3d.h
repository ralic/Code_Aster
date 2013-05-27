        interface
          subroutine nmpl3d(fami,nno,npg,ipoids,ivf,idfde,geom,typmod,&
     &option,imate,compor,lgpg,crit,instam,instap,deplm,deplp,angmas,&
     &sigm,vim,matsym,dfdi,def,sigp,vip,matuu,vectu,codret)
            integer :: lgpg
            integer :: npg
            integer :: nno
            character(*) :: fami
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: geom(3,nno)
            character(len=8) :: typmod(*)
            character(len=16) :: option
            integer :: imate
            character(len=16) :: compor(16)
            real(kind=8) :: crit(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: deplm(1:3,1:nno)
            real(kind=8) :: deplp(1:3,1:nno)
            real(kind=8) :: angmas(*)
            real(kind=8) :: sigm(6,npg)
            real(kind=8) :: vim(lgpg,npg)
            logical :: matsym
            real(kind=8) :: dfdi(nno,3)
            real(kind=8) :: def(6,nno,3)
            real(kind=8) :: sigp(6,npg)
            real(kind=8) :: vip(lgpg,npg)
            real(kind=8) :: matuu(*)
            real(kind=8) :: vectu(3,nno)
            integer :: codret
          end subroutine nmpl3d
        end interface
