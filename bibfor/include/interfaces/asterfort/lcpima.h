        interface
          subroutine lcpima(fami,kpg,ksp,poum,mate,compor,instam,&
     &instap,crit,sigm,vim)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=1) :: poum
            integer :: mate
            character(len=16) :: compor
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: crit(*)
            real(kind=8) :: sigm(*)
            real(kind=8) :: vim(*)
          end subroutine lcpima
        end interface
