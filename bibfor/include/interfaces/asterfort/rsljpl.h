        interface
          subroutine rsljpl(fami,kpg,ksp,loi,imat,nmat,mater,sig,vin,&
     &vind,deps,theta,dt,dsde)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=16) :: loi
            integer :: imat
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: vin(*)
            real(kind=8) :: vind(*)
            real(kind=8) :: deps(6)
            real(kind=8) :: theta
            real(kind=8) :: dt
            real(kind=8) :: dsde(6,6)
          end subroutine rsljpl
        end interface
