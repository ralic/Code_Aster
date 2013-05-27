        interface
          subroutine lcmmat(fami,kpg,ksp,comp,mod,imat,nmat,angmas,pgl&
     &,materd,materf,matcst,nbcomm,cpmono,ndt,ndi,nr,nvi,hsr,nfs,nsg,&
     &toutms,vind,impexp)
            integer :: nsg
            integer :: nfs
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=16) :: comp(*)
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: angmas(3)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            character(len=3) :: matcst
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            integer :: ndt
            integer :: ndi
            integer :: nr
            integer :: nvi
            real(kind=8) :: hsr(nsg,nsg)
            real(kind=8) :: toutms(nfs,nsg,6)
            real(kind=8) :: vind(*)
            integer :: impexp
          end subroutine lcmmat
        end interface
