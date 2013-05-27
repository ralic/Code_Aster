        interface
          subroutine lcmmon(fami,kpg,ksp,comp,nbcomm,cpmono,nmat,nvi,&
     &vini,x,dtime,pgl,mod,coeft,neps,epsd,detot,coel,dvin,nfs,nsg,&
     &toutms,hsr,itmax,toler,iret)
            integer :: nsg
            integer :: nfs
            integer :: neps
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
            real(kind=8) :: pgl(3,3)
            character(len=8) :: mod
            real(kind=8) :: coeft(nmat)
            real(kind=8) :: epsd(neps)
            real(kind=8) :: detot(neps)
            real(kind=8) :: coel(nmat)
            real(kind=8) :: dvin(*)
            real(kind=8) :: toutms(nfs,nsg,6)
            real(kind=8) :: hsr(nsg,nsg,1)
            integer :: itmax
            real(kind=8) :: toler
            integer :: iret
          end subroutine lcmmon
        end interface
