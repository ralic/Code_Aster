        interface
          subroutine comp1d(fami,kpg,ksp,option,sigx,epsx,depx,angmas,&
     &vim,vip,sigxp,etan,codret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=16) :: option
            real(kind=8) :: sigx
            real(kind=8) :: epsx
            real(kind=8) :: depx
            real(kind=8) :: angmas(3)
            real(kind=8) :: vim(*)
            real(kind=8) :: vip(*)
            real(kind=8) :: sigxp
            real(kind=8) :: etan
            integer :: codret
          end subroutine comp1d
        end interface
