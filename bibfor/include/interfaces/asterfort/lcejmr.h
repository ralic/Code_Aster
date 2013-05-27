        interface
          subroutine lcejmr(fami,kpg,ksp,ndim,mate,option,epsm,deps,&
     &sigmo,sigma,dsidep,vim,vip,coorot,typmod,instam,instap)
            integer :: ndim
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: mate
            character(len=16) :: option
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigmo(6)
            real(kind=8) :: sigma(6)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: vim(*)
            real(kind=8) :: vip(*)
            real(kind=8) :: coorot(ndim+ndim*ndim)
            character(len=8) :: typmod(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
          end subroutine lcejmr
        end interface
