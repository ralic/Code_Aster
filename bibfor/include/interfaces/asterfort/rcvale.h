        interface
          subroutine rcvale(nommaz,phenom,nbpar,nompar,valpar,nbres,&
     &nomres,valres,icodre,iarret)
            integer :: nbres
            integer :: nbpar
            character(*) :: nommaz
            character(*) :: phenom
            character(len=8) :: nompar(nbpar)
            real(kind=8) :: valpar(nbpar)
            character(len=8) :: nomres(nbres)
            real(kind=8) :: valres(nbres)
            integer :: icodre(nbres)
            integer :: iarret
          end subroutine rcvale
        end interface
