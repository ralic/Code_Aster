        interface
          subroutine lcmmop(fami,kpg,ksp,comp,nbcomm,cpmono,nmat,nvi,&
     &vini,x,dtime,mod,coeft,epsd,detot,coel,nbphas,nfs,nsg,toutms,dvin,&
     &nhsr,numhsr,hsr,itmax,toler,iret)
            integer :: nhsr
            integer :: nsg
            integer :: nfs
            integer :: nbphas
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=16) :: comp(*)
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            integer :: nvi
            real(kind=8) :: vini(*)
            real(kind=8) :: x
            real(kind=8) :: dtime
            character(len=8) :: mod
            real(kind=8) :: coeft(nmat)
            real(kind=8) :: epsd(6)
            real(kind=8) :: detot(6)
            real(kind=8) :: coel(nmat)
            real(kind=8) :: toutms(nbphas,nfs,nsg,7)
            real(kind=8) :: dvin(*)
            integer :: numhsr(*)
            real(kind=8) :: hsr(nsg,nsg,nhsr)
            integer :: itmax
            real(kind=8) :: toler
            integer :: iret
          end subroutine lcmmop
        end interface
