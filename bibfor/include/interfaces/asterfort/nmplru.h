        interface
          subroutine nmplru(fami,kpg,ksp,poum,ndim,typmod,imate,compor&
     &,ppg,eps,epsp,rp,ener)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(*) :: poum
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: ppg
            real(kind=8) :: eps(6)
            real(kind=8) :: epsp(6)
            real(kind=8) :: rp
            real(kind=8) :: ener(2)
          end subroutine nmplru
        end interface
