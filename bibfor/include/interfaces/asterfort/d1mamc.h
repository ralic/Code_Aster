        interface
          subroutine d1mamc(fami,mater,instan,poum,kpg,ksp,repere,&
     &xyzgau,nbsig,d1)
            integer :: nbsig
            character(*) :: fami
            integer :: mater
            real(kind=8) :: instan
            character(*) :: poum
            integer :: kpg
            integer :: ksp
            real(kind=8) :: repere(7)
            real(kind=8) :: xyzgau(1)
            real(kind=8) :: d1(nbsig,1)
          end subroutine d1mamc
        end interface
