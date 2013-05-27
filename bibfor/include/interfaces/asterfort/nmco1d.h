        interface
          subroutine nmco1d(fami,kpg,ksp,imate,compor,option,epsm,deps&
     &,angmas,sigm,vim,sigp,vip,dsidep,codret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imate
            character(len=16) :: compor(*)
            character(len=16) :: option
            real(kind=8) :: epsm
            real(kind=8) :: deps
            real(kind=8) :: angmas(3)
            real(kind=8) :: sigm
            real(kind=8) :: vim(*)
            real(kind=8) :: sigp
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidep
            integer :: codret
          end subroutine nmco1d
        end interface
