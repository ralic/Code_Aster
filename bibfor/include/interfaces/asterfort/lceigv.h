        interface
          subroutine lceigv(fami,kpg,ksp,neps,imate,compor,epsm,deps,&
     &vim,option,sig,vip,dsidep)
            integer :: neps
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: epsm(neps)
            real(kind=8) :: deps(neps)
            real(kind=8) :: vim(2)
            character(len=16) :: option
            real(kind=8) :: sig(neps)
            real(kind=8) :: vip(2)
            real(kind=8) :: dsidep(neps,neps)
          end subroutine lceigv
        end interface
