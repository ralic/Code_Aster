        interface
          subroutine irmfac(ioccur,formaf,ifichi,niveau,versio,modele,&
     &nomail,nomare,resure,lgmsh)
            integer :: ioccur
            character(len=8) :: formaf
            integer :: ifichi
            integer :: niveau
            integer :: versio
            character(len=8) :: modele
            character(len=8) :: nomail
            character(len=8) :: nomare
            character(len=8) :: resure
            logical :: lgmsh
          end subroutine irmfac
        end interface
