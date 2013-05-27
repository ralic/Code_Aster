        interface
          subroutine eibex(fami,kpg,ksp,ndim,imate,compor,instam,&
     &instap,epsm,deps,vim,option,sig,vip,dsidep,codret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: vim(2)
            character(len=16) :: option
            real(kind=8) :: sig(6)
            real(kind=8) :: vip(2)
            real(kind=8) :: dsidep(6,6)
            integer :: codret
          end subroutine eibex
        end interface
