        interface
          subroutine nmecmi(fami,kpg,ksp,ndim,typmod,imate,compor,crit&
     &,deps,sigm,vim,option,sigp,vip,dsidep,iret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor(3)
            real(kind=8) :: crit(10)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(8)
            character(len=16) :: option
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(8)
            real(kind=8) :: dsidep(6,6)
            integer :: iret
          end subroutine nmecmi
        end interface
