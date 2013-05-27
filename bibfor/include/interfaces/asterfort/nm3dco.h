        interface
          subroutine nm3dco(fami,kpg,ksp,ndim,option,imate,sigm,deps,&
     &vim,sigp,vip,dsidep,crildc,codret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=16) :: option
            integer :: imate
            real(kind=8) :: sigm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: vim(*)
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: crildc(3)
            integer :: codret
          end subroutine nm3dco
        end interface
