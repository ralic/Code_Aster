        interface
          subroutine pmvtgt(option,carcri,deps2,sigp,vip,nbvari,epsilo&
     &,varia,matper,dsidep,smatr,sdeps,ssigp,svip,iret)
            integer :: nbvari
            character(len=16) :: option
            real(kind=8) :: carcri(*)
            real(kind=8) :: deps2(6)
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(nbvari)
            real(kind=8) :: epsilo
            real(kind=8) :: varia(72)
            real(kind=8) :: matper(36)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: smatr(36)
            real(kind=8) :: sdeps(6)
            real(kind=8) :: ssigp(6)
            real(kind=8) :: svip(nbvari)
            integer :: iret
          end subroutine pmvtgt
        end interface
