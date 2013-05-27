        interface
          subroutine lcvali(fami,kpg,ksp,imate,compor,ndim,epsm,deps,&
     &instam,instap,codret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imate
            character(len=16) :: compor(*)
            integer :: ndim
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: instam
            real(kind=8) :: instap
            integer :: codret
          end subroutine lcvali
        end interface
