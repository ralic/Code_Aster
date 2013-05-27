        interface
          subroutine lcedga(fami,kpg,ksp,ndim,imat,crit,typmod,instam,&
     &instap,coord,deps2,sigm2,vim,option,sigp,vip,dsidep,iret)
            integer :: ndim
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imat
            real(kind=8) :: crit(3)
            character(len=8) :: typmod(2)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: coord(3)
            real(kind=8) :: deps2(6)
            real(kind=8) :: sigm2(6)
            real(kind=8) :: vim(2)
            character(len=16) :: option
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(2)
            real(kind=8) :: dsidep(6,6)
            integer :: iret
          end subroutine lcedga
        end interface
