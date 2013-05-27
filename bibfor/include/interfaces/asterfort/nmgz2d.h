        interface
          subroutine nmgz2d(fami,nno,npg,ipoids,ivf,idfde,geomi,typmod&
     &,option,imate,compor,lgpg,crit,instam,instap,ideplm,ideplp,angmas,&
     &sigm,vim,dfdi,pff,def,sigp,vip,matuu,ivectu,codret)
            integer :: lgpg
            integer :: npg
            integer :: nno
            character(*) :: fami
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: geomi(2,nno)
            character(len=8) :: typmod(*)
            character(len=16) :: option
            integer :: imate
            character(len=16) :: compor(4)
            real(kind=8) :: crit(3)
            real(kind=8) :: instam
            real(kind=8) :: instap
            integer :: ideplm
            integer :: ideplp
            real(kind=8) :: angmas(3)
            real(kind=8) :: sigm(4,npg)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: dfdi(nno,2)
            real(kind=8) :: pff(4,nno,nno)
            real(kind=8) :: def(4,nno,2)
            real(kind=8) :: sigp(4,npg)
            real(kind=8) :: vip(lgpg,npg)
            real(kind=8) :: matuu(*)
            integer :: ivectu
            integer :: codret
          end subroutine nmgz2d
        end interface
