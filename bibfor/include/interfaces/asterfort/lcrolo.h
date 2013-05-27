        interface
          subroutine lcrolo(fami,kpg,ksp,mate,option,carcri,fm,df,vim,&
     &vip,taup,dtaudf,iret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: mate
            character(len=16) :: option
            real(kind=8) :: carcri(3)
            real(kind=8) :: fm(3,3)
            real(kind=8) :: df(3,3)
            real(kind=8) :: vim(9)
            real(kind=8) :: vip(9)
            real(kind=8) :: taup(6)
            real(kind=8) :: dtaudf(6,3,3)
            integer :: iret
          end subroutine lcrolo
        end interface
