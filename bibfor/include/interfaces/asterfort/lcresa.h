        interface
          subroutine lcresa(fami,kpg,ksp,typmod,imat,nmat,materd,&
     &materf,comp,nr,nvi,timed,timef,deps,epsd,yf,dy,r,iret,yd,crit)
            integer :: nvi
            integer :: nr
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=8) :: typmod
            integer :: imat
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            character(len=16) :: comp(*)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: deps(6)
            real(kind=8) :: epsd(6)
            real(kind=8) :: yf(nr)
            real(kind=8) :: dy(nr)
            real(kind=8) :: r(nr)
            integer :: iret
            real(kind=8) :: yd(*)
            real(kind=8) :: crit(*)
          end subroutine lcresa
        end interface
