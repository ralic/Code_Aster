        interface
          subroutine lcreli(fami,kpg,ksp,loi,mod,imat,nmat,materd,&
     &materf,comp,nbcomm,cpmono,pgl,nfs,nsg,toutms,hsr,nr,nvi,vind,vinf,&
     &itmax,toler,timed,timef,yd,yf,deps,epsd,dy,r,ddy,iret,crit,indi)
            integer :: nr
            integer :: nsg
            integer :: nfs
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=16) :: loi
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            character(len=16) :: comp(*)
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: toutms(nfs,nsg,6)
            real(kind=8) :: hsr(nsg,nsg)
            integer :: nvi
            real(kind=8) :: vind(*)
            real(kind=8) :: vinf(*)
            integer :: itmax
            real(kind=8) :: toler
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: yd(*)
            real(kind=8) :: yf(*)
            real(kind=8) :: deps(6)
            real(kind=8) :: epsd(6)
            real(kind=8) :: dy(*)
            real(kind=8) :: r(*)
            real(kind=8) :: ddy(*)
            integer :: iret
            real(kind=8) :: crit(*)
            integer :: indi(7)
          end subroutine lcreli
        end interface
