        interface
          subroutine gerpas(fami,kpg,ksp,comp,mod,imat,matcst,nbcomm,&
     &cpmono,nbphas,nvi,nmat,y,pas,itmax,eps,toly,cothe,coeff,dcothe,&
     &dcoeff,coel,pgl,angmas,neps,epsd,detot,x,nfs,nsg,nhsr,numhsr,hsr,&
     &iret)
            integer :: nhsr
            integer :: nsg
            integer :: nfs
            integer :: neps
            integer :: nmat
            integer :: nvi
            integer :: nbphas
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=16) :: comp(*)
            character(len=8) :: mod
            integer :: imat
            character(len=3) :: matcst
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: y(nvi)
            real(kind=8) :: pas
            integer :: itmax
            real(kind=8) :: eps
            real(kind=8) :: toly
            real(kind=8) :: cothe(nmat)
            real(kind=8) :: coeff(nmat)
            real(kind=8) :: dcothe(nmat)
            real(kind=8) :: dcoeff(nmat)
            real(kind=8) :: coel(nmat)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: angmas(3)
            real(kind=8) :: epsd(neps)
            real(kind=8) :: detot(neps)
            real(kind=8) :: x
            integer :: numhsr(*)
            real(kind=8) :: hsr(nsg,nsg,nhsr)
            integer :: iret
          end subroutine gerpas
        end interface
