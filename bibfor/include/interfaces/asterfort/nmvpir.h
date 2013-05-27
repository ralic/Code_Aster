        interface
          subroutine nmvpir(fami,kpg,ksp,ndim,typmod,imate,compor,crit&
     &,instam,instap,deps,sigm,vim,option,angmas,nvi,sigp,vip,dsidep,&
     &iret)
            integer :: nvi
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: crit(4)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: deps(6)
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(nvi)
            character(len=16) :: option
            real(kind=8) :: angmas(3)
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(nvi)
            real(kind=8) :: dsidep(6,6)
            integer :: iret
          end subroutine nmvpir
        end interface
