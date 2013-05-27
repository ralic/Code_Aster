        interface
          subroutine nmasse(fami,kpg,ksp,poum,icodma,materi,inst,e,nu,&
     &deuxmu,troisk)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(*) :: poum
            integer :: icodma
            character(*) :: materi
            real(kind=8) :: inst
            real(kind=8) :: e
            real(kind=8) :: nu
            real(kind=8) :: deuxmu
            real(kind=8) :: troisk
          end subroutine nmasse
        end interface
