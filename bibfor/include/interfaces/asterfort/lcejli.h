        interface
          subroutine lcejli(fami,kpg,ksp,ndim,mate,option,am,da,sigma,&
     &dsidep,vim,vip)
            integer :: ndim
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: mate
            character(len=16) :: option
            real(kind=8) :: am(ndim)
            real(kind=8) :: da(ndim)
            real(kind=8) :: sigma(6)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: vim(*)
            real(kind=8) :: vip(*)
          end subroutine lcejli
        end interface
