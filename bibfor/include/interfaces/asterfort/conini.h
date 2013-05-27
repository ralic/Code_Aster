        interface
          subroutine conini(ma,noecon,maicon,marcon,nbmar,nbnoe,nbmarc&
     &,nommar,jmicor,mbcor,nomtyr,nbgco,io8gco)
            integer :: nbnoe
            integer :: nbmar
            character(len=8) :: ma
            integer :: noecon(nbnoe)
            integer :: maicon(nbmar)
            integer :: marcon(nbmar)
            integer :: nbmarc
            character(len=8) :: nommar(nbmar)
            integer :: jmicor(nbmar)
            integer :: mbcor(nbmar)
            character(len=8) :: nomtyr(nbmar)
            integer :: nbgco
            integer :: io8gco
          end subroutine conini
        end interface
