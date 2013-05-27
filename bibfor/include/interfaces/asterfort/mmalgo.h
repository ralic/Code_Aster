        interface
          subroutine mmalgo(indcoi,lvites,lglini,jeu,jeuvit,lambdc,&
     &coefac,ctcsta,mmcvca,scotch,indcon)
            integer :: indcoi
            logical :: lvites
            logical :: lglini
            real(kind=8) :: jeu
            real(kind=8) :: jeuvit
            real(kind=8) :: lambdc
            real(kind=8) :: coefac
            integer :: ctcsta
            logical :: mmcvca
            logical :: scotch
            integer :: indcon
          end subroutine mmalgo
        end interface
