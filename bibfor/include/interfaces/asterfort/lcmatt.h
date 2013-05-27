        interface
          subroutine lcmatt(fami,kpg,ksp,mod,imat,nmat,poum,comp,&
     &coefel,coefpl,typma,ndt,ndi,nr,nvi)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=8) :: mod
            integer :: imat
            character(*) :: poum
            character(len=16) :: comp(*)
            real(kind=8) :: coefel(nmat)
            real(kind=8) :: coefpl(nmat)
            character(len=8) :: typma
            integer :: ndt
            integer :: ndi
            integer :: nr
            integer :: nvi
          end subroutine lcmatt
        end interface
