        interface
          subroutine lcjacb(fami,kpg,ksp,loi,mod,nmat,materd,materf,&
     &timed,timef,yf,deps,itmax,toler,nbcomm,cpmono,pgl,nfs,nsg,toutms,&
     &hsr,nr,comp,nvi,vind,vinf,epsd,yd,dy,ye,crit,indi,vind1,bnews,&
     &mtrac,drdy,iret)
            integer :: nvi
            integer :: nr
            integer :: nsg
            integer :: nfs
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=16) :: loi
            character(len=8) :: mod
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: yf(nr)
            real(kind=8) :: deps(6)
            integer :: itmax
            real(kind=8) :: toler
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: toutms(nfs,nsg,6)
            real(kind=8) :: hsr(nsg,nsg)
            character(len=16) :: comp(*)
            real(kind=8) :: vind(*)
            real(kind=8) :: vinf(*)
            real(kind=8) :: epsd(6)
            real(kind=8) :: yd(nr)
            real(kind=8) :: dy(nr)
            real(kind=8) :: ye(nr)
            real(kind=8) :: crit(*)
            integer :: indi(7)
            real(kind=8) :: vind1(nvi)
            logical :: bnews(3)
            logical :: mtrac
            real(kind=8) :: drdy(nr,nr)
            integer :: iret
          end subroutine lcjacb
        end interface
