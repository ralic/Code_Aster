        interface
          subroutine cvmmat(fami,kpg,ksp,mod,imat,nmat,materd,materf,&
     &matcst,typma,ndt,ndi,nr,crit,vim,nvi,sigd)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            character(len=3) :: matcst
            character(len=8) :: typma
            integer :: ndt
            integer :: ndi
            integer :: nr
            real(kind=8) :: crit(*)
            real(kind=8) :: vim(*)
            integer :: nvi
            real(kind=8) :: sigd(6)
          end subroutine cvmmat
        end interface
