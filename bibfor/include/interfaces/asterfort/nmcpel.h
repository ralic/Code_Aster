        interface
          subroutine nmcpel(fami,kpg,ksp,poum,ndim,typmod,angmas,imate&
     &,compor,crit,option,eps,sig,vi,dsidep,codret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(*) :: poum
            integer :: ndim
            character(len=8) :: typmod(*)
            real(kind=8) :: angmas(3)
            integer :: imate
            character(len=16) :: compor(4)
            real(kind=8) :: crit(3)
            character(len=16) :: option
            real(kind=8) :: eps(6)
            real(kind=8) :: sig(6)
            real(kind=8) :: vi(*)
            real(kind=8) :: dsidep(6,6)
            integer :: codret
          end subroutine nmcpel
        end interface
