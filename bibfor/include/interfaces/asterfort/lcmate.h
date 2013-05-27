        interface
          subroutine lcmate(fami,kpg,ksp,comp,mod,imat,nmat,tempd,&
     &tempf,impexp,typma,hsr,materd,materf,matcst,nbcomm,cpmono,angmas,&
     &pgl,itmax,toler,ndt,ndi,nr,crit,nvi,vind,nfs,nsg,toutms,nhsr,&
     &numhsr,sigd)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=16) :: comp(*)
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: tempd
            real(kind=8) :: tempf
            integer :: impexp
            character(len=8) :: typma
            real(kind=8) :: hsr(*)
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            character(len=3) :: matcst
            integer :: nbcomm(*)
            character(len=24) :: cpmono(*)
            real(kind=8) :: angmas(3)
            real(kind=8) :: pgl(3,3)
            integer :: itmax
            real(kind=8) :: toler
            integer :: ndt
            integer :: ndi
            integer :: nr
            real(kind=8) :: crit(*)
            integer :: nvi
            real(kind=8) :: vind(*)
            integer :: nfs
            integer :: nsg
            real(kind=8) :: toutms(*)
            integer :: nhsr
            integer :: numhsr(*)
            real(kind=8) :: sigd(6)
          end subroutine lcmate
        end interface
