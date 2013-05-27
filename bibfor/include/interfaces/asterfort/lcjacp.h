        interface
          subroutine lcjacp(fami,kpg,ksp,loi,toler,itmax,mod,imat,nmat&
     &,materd,materf,nr,nvi,timed,timef,deps,epsd,vind,vinf,yd,comp,&
     &nbcomm,cpmono,pgl,nfs,nsg,toutms,hsr,dy,r,drdy,verjac,drdyb,iret,&
     &crit,indi)
            integer :: nsg
            integer :: nfs
            integer :: nvi
            integer :: nr
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=16) :: loi
            real(kind=8) :: toler
            integer :: itmax
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: deps(6)
            real(kind=8) :: epsd(6)
            real(kind=8) :: vind(nvi)
            real(kind=8) :: vinf(nvi)
            real(kind=8) :: yd(nr)
            character(len=16) :: comp(*)
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: toutms(nfs,nsg,6)
            real(kind=8) :: hsr(nsg,nsg)
            real(kind=8) :: dy(nr)
            real(kind=8) :: r(nr)
            real(kind=8) :: drdy(nr,nr)
            integer :: verjac
            real(kind=8) :: drdyb(nr,nr)
            integer :: iret
            real(kind=8) :: crit(*)
            integer :: indi(7)
          end subroutine lcjacp
        end interface
