        interface
          subroutine calsig(fami,kpg,ksp,ein,mod,comp,vini,x,dtime,&
     &epsd,detot,nmat,coel,sigi)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            real(kind=8) :: ein(6)
            character(len=8) :: mod
            character(len=16) :: comp(*)
            real(kind=8) :: vini(*)
            real(kind=8) :: x
            real(kind=8) :: dtime
            real(kind=8) :: epsd(6)
            real(kind=8) :: detot(6)
            real(kind=8) :: coel(nmat)
            real(kind=8) :: sigi(6)
          end subroutine calsig
        end interface
