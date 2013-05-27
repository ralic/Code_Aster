        interface
          subroutine nm1vil(fami,kpg,ksp,icdmat,materi,crit,instam,&
     &instap,tm,tp,tref,deps,sigm,vim,option,defam,defap,angmas,sigp,vip&
     &,dsidep,iret,compo,nbvalc)
            integer :: nbvalc
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: icdmat
            character(len=8) :: materi
            real(kind=8) :: crit(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: tm
            real(kind=8) :: tp
            real(kind=8) :: tref
            real(kind=8) :: deps
            real(kind=8) :: sigm
            real(kind=8) :: vim(nbvalc)
            character(len=16) :: option
            real(kind=8) :: defam
            real(kind=8) :: defap
            real(kind=8) :: angmas(3)
            real(kind=8) :: sigp
            real(kind=8) :: vip(nbvalc)
            real(kind=8) :: dsidep
            integer :: iret
            character(len=16) :: compo
          end subroutine nm1vil
        end interface
