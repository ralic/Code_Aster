        interface
          subroutine lcesgv(fami,kpg,ksp,neps,typmod,option,mat,epsm,&
     &deps,vim,itemax,precvg,sig,vip,dsidep,iret)
            integer :: neps
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=8) :: typmod
            character(len=16) :: option
            integer :: mat
            real(kind=8) :: epsm(neps)
            real(kind=8) :: deps(neps)
            real(kind=8) :: vim(*)
            integer :: itemax
            real(kind=8) :: precvg
            real(kind=8) :: sig(neps)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidep(neps,neps)
            integer :: iret
          end subroutine lcesgv
        end interface
