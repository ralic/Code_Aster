        interface
          subroutine recugd(caelem,nomcmp,valres,nbgd,iassef,iassmx)
            integer :: iassef
            integer :: nbgd
            character(len=19) :: caelem
            character(len=8) :: nomcmp(nbgd)
            real(kind=8) :: valres(nbgd*iassef)
            integer :: iassmx
          end subroutine recugd
        end interface
