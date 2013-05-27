        interface
          subroutine rkdvec(fami,kpg,ksp,imat,matcst,nvi,vini,coeft,x,&
     &dtime,nmat,sigi,dvin)
            integer :: nmat
            integer :: nvi
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imat
            character(len=3) :: matcst
            real(kind=8) :: vini(nvi)
            real(kind=8) :: coeft(nmat)
            real(kind=8) :: x
            real(kind=8) :: dtime
            real(kind=8) :: sigi(6)
            real(kind=8) :: dvin(nvi)
          end subroutine rkdvec
        end interface
