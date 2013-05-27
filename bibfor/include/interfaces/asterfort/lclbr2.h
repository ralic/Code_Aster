        interface
          subroutine lclbr2(fami,kpg,ksp,imate,compor,ndim,epsm,t,e,&
     &sigmt,sigmc,epsic,compn,gamma)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imate
            character(len=16) :: compor(*)
            integer :: ndim
            real(kind=8) :: epsm(6)
            integer :: t(3,3)
            real(kind=8) :: e
            real(kind=8) :: sigmt
            real(kind=8) :: sigmc
            real(kind=8) :: epsic
            real(kind=8) :: compn
            real(kind=8) :: gamma
          end subroutine lclbr2
        end interface
