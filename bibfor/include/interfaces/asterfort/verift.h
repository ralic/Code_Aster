        interface
          subroutine verift(fami,kpg,ksp,poum,imate,compor,ndim,epsth,&
     &iret)
            integer :: ndim
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(*) :: poum
            integer :: imate
            character(*) :: compor
            real(kind=8) :: epsth(ndim)
            integer :: iret
          end subroutine verift
        end interface
