        interface
          subroutine lcrous(fami,kpg,ksp,toler,itmax,imat,nmat,materd,&
     &materf,nvi,deps,sigd,vind,theta,loi,dt,sigf,vinf,irtet)
            integer :: nvi
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            real(kind=8) :: toler
            integer :: itmax
            integer :: imat
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: vind(nvi)
            real(kind=8) :: theta
            character(len=16) :: loi
            real(kind=8) :: dt
            real(kind=8) :: sigf(6)
            real(kind=8) :: vinf(nvi)
            integer :: irtet
          end subroutine lcrous
        end interface
