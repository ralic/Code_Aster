        interface
          subroutine lceiab(fami,kpg,ksp,mat,option,mu,su,de,ddedt,vim&
     &,vip,r,codret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: mat
            character(len=16) :: option
            real(kind=8) :: mu(3)
            real(kind=8) :: su(3)
            real(kind=8) :: de(6)
            real(kind=8) :: ddedt(6,6)
            real(kind=8) :: vim(*)
            real(kind=8) :: vip(*)
            real(kind=8) :: r
            integer :: codret
          end subroutine lceiab
        end interface
