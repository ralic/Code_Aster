        interface
          subroutine plasti(fami,kpg,ksp,typmod,imat,comp,crit,timed,&
     &timef,tempd,tempf,tref,epsdt,depst,sigd,vind,opt,angmas,sigf,vinf,&
     &dsde,icomp,nvi,tampon,irteti)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=8) :: typmod(*)
            integer :: imat
            character(len=16) :: comp(*)
            real(kind=8) :: crit(*)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: tempd
            real(kind=8) :: tempf
            real(kind=8) :: tref
            real(kind=8) :: epsdt(9)
            real(kind=8) :: depst(9)
            real(kind=8) :: sigd(6)
            real(kind=8) :: vind(*)
            character(len=16) :: opt
            real(kind=8) :: angmas(3)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vinf(*)
            real(kind=8) :: dsde(6,*)
            integer :: icomp
            integer :: nvi
            real(kind=8) :: tampon(*)
            integer :: irteti
          end subroutine plasti
        end interface
