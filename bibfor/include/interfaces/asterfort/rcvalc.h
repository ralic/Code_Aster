        interface
          subroutine rcvalc(jmat,phenom,nbres,nomres,valres,icodre,&
     &iarret)
            integer :: nbres
            integer :: jmat
            character(*) :: phenom
            character(*) :: nomres(nbres)
            complex(kind=8) :: valres(nbres)
            integer :: icodre(nbres)
            integer :: iarret
          end subroutine rcvalc
        end interface
