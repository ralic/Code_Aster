        interface
          subroutine lcmmap(fami,kpg,ksp,comp,mod,imat,nmat,angmas,pgl&
     &,materd,materf,matcst,nbcomm,cpmono,ndt,ndi,nr,nvi,nfs,nsg,nhsr,&
     &numhsr,hsr)
            integer :: nhsr
            integer :: nsg
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
            integer :: nfs
            integer :: numhsr(nhsr)
            real(kind=8) :: hsr(nsg,nsg,nhsr)
          end subroutine lcmmap
        end interface
