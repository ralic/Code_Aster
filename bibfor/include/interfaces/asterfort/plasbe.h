        interface
          subroutine plasbe(fami,kpg,ksp,typmod,imat,crit,epsdt,depst,&
     &sigd,vind,opt,elgeom,sigf,vinf,dsde,icomp,nvi,irteti)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=8) :: typmod(*)
            integer :: imat
            real(kind=8) :: crit(*)
            real(kind=8) :: epsdt(6)
            real(kind=8) :: depst(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: vind(*)
            character(len=16) :: opt
            real(kind=8) :: elgeom(*)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vinf(*)
            real(kind=8) :: dsde(6,6)
            integer :: icomp
            integer :: nvi
            integer :: irteti
          end subroutine plasbe
        end interface
