        interface
          subroutine mmopti(loptin,resoco,seuili,ctcini,lgliss,iptc,&
     &epsint,jeusgn)
            logical :: loptin
            character(len=24) :: resoco
            real(kind=8) :: seuili
            integer :: ctcini
            logical :: lgliss
            integer :: iptc
            real(kind=8) :: epsint
            real(kind=8) :: jeusgn
          end subroutine mmopti
        end interface
