        interface
          subroutine rcvala(jmat,nomat,phenom,nbpar,nompar,valpar,&
     &nbres,nomres,valres,icodre,iarret)
            integer :: nbres
            integer :: nbpar
            integer :: jmat
            character(*) :: nomat
            character(*) :: phenom
            character(*) :: nompar(nbpar)
            real(kind=8) :: valpar(nbpar)
            character(*) :: nomres(nbres)
            real(kind=8) :: valres(nbres)
            integer :: icodre(nbres)
            integer :: iarret
          end subroutine rcvala
        end interface
