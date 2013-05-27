        interface
          subroutine lc0001(fami,kpg,ksp,ndim,imate,neps,deps,nsig,&
     &sigm,option,angmas,sigp,vip,typmod,ndsde,dsidep,codret)
            integer :: ndsde
            integer :: nsig
            integer :: neps
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            integer :: imate
            real(kind=8) :: deps(neps)
            real(kind=8) :: sigm(nsig)
            character(len=16) :: option
            real(kind=8) :: angmas(3)
            real(kind=8) :: sigp(nsig)
            real(kind=8) :: vip(1)
            character(len=8) :: typmod(*)
            real(kind=8) :: dsidep(ndsde)
            integer :: codret
          end subroutine lc0001
        end interface
