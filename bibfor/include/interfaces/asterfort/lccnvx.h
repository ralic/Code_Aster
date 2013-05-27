        interface
          subroutine lccnvx(fami,kpg,ksp,loi,mod,imat,nmat,materd,&
     &materf,sigd,sigf,deps,vind,vinf,nbcomm,cpmono,pgl,nvi,vp,vecp,hsr,&
     &nfs,nsg,toutms,timed,timef,nr,yd,yf,toler,seuil,iret)
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
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: vind(*)
            real(kind=8) :: vinf(*)
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: pgl(3,3)
            integer :: nvi
            real(kind=8) :: vp(3)
            real(kind=8) :: vecp(3,3)
            real(kind=8) :: hsr(nsg,nsg)
            real(kind=8) :: toutms(nfs,nsg,6)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: yd(nr)
            real(kind=8) :: yf(nr)
            real(kind=8) :: toler
            real(kind=8) :: seuil
            integer :: iret
          end subroutine lccnvx
        end interface
