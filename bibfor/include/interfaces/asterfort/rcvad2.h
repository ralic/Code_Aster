        interface
          subroutine rcvad2(fami,kpg,ksp,poum,jmat,phenom,nbres,nomres&
     &,valres,devres,icodre)
            integer :: nbres
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(*) :: poum
            integer :: jmat
            character(*) :: phenom
            character(len=8) :: nomres(nbres)
            real(kind=8) :: valres(nbres)
            real(kind=8) :: devres(nbres)
            integer :: icodre(nbres)
          end subroutine rcvad2
        end interface
