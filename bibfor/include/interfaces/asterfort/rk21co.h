        interface
          subroutine rk21co(fami,kpg,ksp,comp,mod,imat,matcst,nbcomm,&
     &cpmono,nfs,nsg,toutms,nvi,nmat,y,kp,ee,a,h,pgl,nbphas,cothe,coeff,&
     &dcothe,dcoeff,coel,x,pas,neps,epsd,detot,nhsr,numhsr,hsr,itmax,&
     &toler,iret)
            integer :: nhsr
            integer :: nmat
            integer :: nvi
            integer :: nsg
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=16) :: comp(*)
            character(len=8) :: mod
            integer :: imat
            character(len=3) :: matcst
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            integer :: nfs
            real(kind=8) :: toutms(*)
            real(kind=8) :: y(nvi)
            integer :: kp
            real(kind=8) :: ee(nvi)
            real(kind=8) :: a(nvi)
            real(kind=8) :: h
            real(kind=8) :: pgl(3,3)
            integer :: nbphas
            real(kind=8) :: cothe(nmat)
            real(kind=8) :: coeff(nmat)
            real(kind=8) :: dcothe(nmat)
            real(kind=8) :: dcoeff(nmat)
            real(kind=8) :: coel(nmat)
            real(kind=8) :: x
            real(kind=8) :: pas
            integer :: neps
            real(kind=8) :: epsd(6)
            real(kind=8) :: detot(6)
            integer :: numhsr(*)
            real(kind=8) :: hsr(nsg,nsg,nhsr)
            integer :: itmax
            real(kind=8) :: toler
            integer :: iret
          end subroutine rk21co
        end interface
