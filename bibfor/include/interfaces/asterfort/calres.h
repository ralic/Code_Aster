        interface
          subroutine calres(np3,ic,typch,nbseg,choc,rc,theta,vloc,xloc&
     &,vloc0,xloc0,excloc,tetaj,jacobc,jacobk,floc,flres,old,oldia,iforn&
     &,toln)
            integer :: np3
            integer :: ic
            integer :: typch(*)
            integer :: nbseg(*)
            real(kind=8) :: choc(6,*)
            real(kind=8) :: rc(np3,*)
            real(kind=8) :: theta(np3,*)
            real(kind=8) :: vloc(*)
            real(kind=8) :: xloc(*)
            real(kind=8) :: vloc0(*)
            real(kind=8) :: xloc0(*)
            real(kind=8) :: excloc(*)
            real(kind=8) :: tetaj
            real(kind=8) :: jacobc(3,*)
            real(kind=8) :: jacobk(3,*)
            real(kind=8) :: floc(*)
            real(kind=8) :: flres(*)
            real(kind=8) :: old(9,*)
            integer :: oldia(*)
            integer :: iforn
            real(kind=8) :: toln
          end subroutine calres
        end interface
