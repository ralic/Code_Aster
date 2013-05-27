        interface
          subroutine lcdvin(fami,kpg,ksp,comp,mod,imat,matcst,nvi,nmat&
     &,vini,coeft,x,dtime,sigi,dvin,iret)
            integer :: nmat
            integer :: nvi
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=16) :: comp(*)
            character(len=8) :: mod
            integer :: imat
            character(len=3) :: matcst
            real(kind=8) :: vini(nvi)
            real(kind=8) :: coeft(nmat)
            real(kind=8) :: x
            real(kind=8) :: dtime
            real(kind=8) :: sigi(6)
            real(kind=8) :: dvin(nvi)
            integer :: iret
          end subroutine lcdvin
        end interface
