        interface
          subroutine rcvada(jmat,phenom,temp,nbres,nomres,valres,&
     &devres,icodre)
            integer :: nbres
            integer :: jmat
            character(*) :: phenom
            real(kind=8) :: temp
            character(len=8) :: nomres(nbres)
            real(kind=8) :: valres(nbres)
            real(kind=8) :: devres(nbres)
            integer :: icodre(nbres)
          end subroutine rcvada
        end interface
