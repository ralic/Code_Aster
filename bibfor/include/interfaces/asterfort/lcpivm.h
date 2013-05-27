        interface
          subroutine lcpivm(fami,kpg,ksp,mate,compor,carcri,instam,&
     &instap,fm,df,vim,option,taup,vip,dtaudf,iret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: mate
            character(len=16) :: compor
            real(kind=8) :: carcri(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: fm(3,3)
            real(kind=8) :: df(3,3)
            real(kind=8) :: vim(8)
            character(len=16) :: option
            real(kind=8) :: taup(6)
            real(kind=8) :: vip(8)
            real(kind=8) :: dtaudf(6,3,3)
            integer :: iret
          end subroutine lcpivm
        end interface
