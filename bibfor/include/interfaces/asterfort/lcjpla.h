        interface
          subroutine lcjpla(fami,kpg,ksp,loi,mod,nr,imat,nmat,mater,&
     &nvi,deps,sigf,vin,dsde,sigd,vind,vp,vecp,theta,dt,devg,devgii,&
     &codret)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=16) :: loi
            character(len=8) :: mod
            integer :: nr
            integer :: imat
            real(kind=8) :: mater(nmat,2)
            integer :: nvi
            real(kind=8) :: deps(6)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vin(*)
            real(kind=8) :: dsde(6,6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: vind(*)
            real(kind=8) :: vp(3)
            real(kind=8) :: vecp(3,3)
            real(kind=8) :: theta
            real(kind=8) :: dt
            real(kind=8) :: devg(*)
            real(kind=8) :: devgii
            integer :: codret
          end subroutine lcjpla
        end interface
