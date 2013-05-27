        interface
          subroutine lceifa(fami,kpg,ksp,mat,option,mu,su,deltap,ddedt&
     &,vim,vip,r)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: mat
            character(len=16) :: option
            real(kind=8) :: mu(3)
            real(kind=8) :: su(3)
            real(kind=8) :: deltap(6)
            real(kind=8) :: ddedt(6,6)
            real(kind=8) :: vim(*)
            real(kind=8) :: vip(*)
            real(kind=8) :: r
          end subroutine lceifa
        end interface
