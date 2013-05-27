        interface
          subroutine comcq1(fami,kpg,ksp,mod,imate,compor,carcri,instm&
     &,instp,eps,deps,tempm,tempp,sigm,vim,option,angmas,sigp,vip,dsde,&
     &codret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: mod
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: carcri(*)
            real(kind=8) :: instm
            real(kind=8) :: instp
            real(kind=8) :: eps(4)
            real(kind=8) :: deps(4)
            real(kind=8) :: tempm
            real(kind=8) :: tempp
            real(kind=8) :: sigm(4)
            real(kind=8) :: vim(*)
            character(len=16) :: option
            real(kind=8) :: angmas(3)
            real(kind=8) :: sigp(4)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsde(6,6)
            integer :: codret
          end subroutine comcq1
        end interface
