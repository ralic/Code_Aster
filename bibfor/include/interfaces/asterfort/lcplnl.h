        interface
          subroutine lcplnl(fami,kpg,ksp,loi,toler,itmax,mod,imat,nmat&
     &,materd,materf,nr,nvi,timed,timef,deps,epsd,sigd,vind,comp,nbcomm,&
     &cpmono,pgl,nfs,nsg,toutms,hsr,sigf,vinf,icomp,codret,drdy,tampon,&
     &crit)
            common/tdim/ ndt,ndi
              integer :: ndt
              integer :: ndi
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
            real(kind=8) :: deps(*)
            real(kind=8) :: epsd(*)
            real(kind=8) :: sigd(6)
            real(kind=8) :: vind(*)
            character(len=16) :: comp(*)
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: toutms(nfs,nsg,6)
            real(kind=8) :: hsr(nsg,nsg)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vinf(*)
            integer :: icomp
            integer :: codret
            real(kind=8) :: drdy(nr,nr)
            real(kind=8) :: tampon(*)
            real(kind=8) :: crit(*)
          end subroutine lcplnl
        end interface
