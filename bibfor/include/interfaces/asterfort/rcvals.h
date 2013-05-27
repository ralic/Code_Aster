        interface
          subroutine rcvals(iarret,icodre,nbres,nomres)
            integer :: nbres
            integer :: iarret
            integer :: icodre(nbres)
            character(*) :: nomres(nbres)
          end subroutine rcvals
        end interface
