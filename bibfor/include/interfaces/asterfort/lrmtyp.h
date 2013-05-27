        interface
          subroutine lrmtyp(nbtyp,nomtyp,nnotyp,typgeo,renumd,modnum,&
     &nuanom,numnoa)
            integer :: nbtyp
            character(len=8) :: nomtyp(69)
            integer :: nnotyp(69)
            integer :: typgeo(69)
            integer :: renumd(69)
            integer :: modnum(69)
            integer :: nuanom(69,27)
            integer :: numnoa(69,27)
          end subroutine lrmtyp
        end interface
