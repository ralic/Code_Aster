        interface
          subroutine nm1dpm(fami,kpg,ksp,imate,option,nvar,ncstpm,&
     &cstpm,sigm,vim,deps,vip,sigp,dsde)
            integer :: ncstpm
            integer :: nvar
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imate
            character(*) :: option
            real(kind=8) :: cstpm(ncstpm)
            real(kind=8) :: sigm
            real(kind=8) :: vim(nvar)
            real(kind=8) :: deps
            real(kind=8) :: vip(nvar)
            real(kind=8) :: sigp
            real(kind=8) :: dsde
          end subroutine nm1dpm
        end interface
