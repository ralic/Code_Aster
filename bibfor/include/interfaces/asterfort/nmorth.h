        interface
          subroutine nmorth(fami,kpg,ksp,ndim,phenom,imate,poum,deps,&
     &sigm,option,angmas,sigp,vip,dsidep)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=16) :: phenom
            integer :: imate
            character(*) :: poum
            real(kind=8) :: deps(6)
            real(kind=8) :: sigm(6)
            character(len=16) :: option
            real(kind=8) :: angmas(3)
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip
            real(kind=8) :: dsidep(6,6)
          end subroutine nmorth
        end interface
