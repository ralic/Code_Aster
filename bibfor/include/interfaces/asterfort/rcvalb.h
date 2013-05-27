        interface
          subroutine rcvalb(fami,kpg,ksp,poum,jmat,nomat,phenom,nbpar,&
     &nompar,valpar,nbres,nomres,valres,codret,iarret)
            integer :: nbres
            integer :: nbpar
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(*) :: poum
            integer :: jmat
            character(*) :: nomat
            character(*) :: phenom
            character(*) :: nompar(nbpar)
            real(kind=8) :: valpar(nbpar)
            character(*) :: nomres(nbres)
            real(kind=8) :: valres(nbres)
            integer :: codret(nbres)
            integer :: iarret
          end subroutine rcvalb
        end interface
